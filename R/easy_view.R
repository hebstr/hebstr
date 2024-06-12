#' Title
#'
#' @param x
#' @param name
#' @param width
#' @param color_type
#' @param focus_na
#' @param ...
#' @param font_size
#' @param font_familiy
#' @param palette
#'
#' @return
#' @export
#'
#' @examples
#'
easy_view <- \(x,
               name = NULL,
               width = 600,
               font_size = 12,
               font_family = "arial",
               color_type = FALSE,
               palette = "viridis",
               focus_na = FALSE,
               ...) {

  if (is.null(name)) name <- rlang::enexpr(x)

  set_cols <- \(y, vars, fn, name) {

    vars <- rlang::enexprs(vars)
    fn <- rlang::enexpr(fn)

    data <-
    y |>
      dplyr::select(!!!vars) |>
      names() |>
      rlang::set_names() |>
      purrr::map(~ eval(fn))

    data <- tibble::tibble(variable = names(data), !!name := data)

  }

  .cols <-
  list(range =
         set_cols(x,
                  vars = dplyr::where(is.numeric) | dplyr::where(is.Date),
                  fn = x[[.]] |> range(na.rm = TRUE) |> round(1),
                  name = "range"),
       q1_q2_q3 =
         set_cols(x,
                  vars = dplyr::where(is.numeric),
                  fn = stats::quantile(x[[.]],
                                       probs = c(.25, .5, .75),
                                       na.rm = TRUE) |> round(1),
                  name = "q1_q2_q3"),
       bin =
         set_cols(x,
                  vars = dplyr::where(is.numeric),
                  fn = length(unique(na.omit(x[[.]]))) == 2,
                  name = "bin"))

  .view <-
  dplyr::lst(data =
             .cols |>
               purrr::reduce(dplyr::left_join,
                             by = dplyr::join_by(variable),
                             .init = labelled::generate_dictionary(x) |> tibble::tibble()) |>
               dplyr::rename(type = col_type,
                             na = missing) |>
               dplyr::mutate(type = dplyr::case_when(bin == "TRUE" ~ "bin",
                                                     bin == "FALSE" ~ "num",
                                                     .default = type),
                             na = dplyr::na_if(na, 0),
                             q1_q2_q3 = dplyr::if_else(type == "bin", NA, q1_q2_q3)) |>
               dplyr::mutate(na_prop = scales::percent(na / nrow(x), accuracy = .1),
                             .after = na) |>
               dplyr::relocate(range, q1_q2_q3, .before = levels) |>
               dplyr::select(dplyr::where(~ !is.null(unlist(.))), -bin),
           output =
             data |>
               gt::gt(rowname_col = "pos") |>
               gt::sub_missing(missing_text = "") |>
               gt::tab_options(data_row.padding = 2,
                               table.width = gt::px(width),
                               table.font.size = gt::px(font_size)) |>
               gt::tab_style(style = gt::cell_text(weight = "bold"),
                             locations = gt::cells_column_labels()) |>
               gt::opt_table_font(font = font_family))

  if (color_type) {

    .view$output <-
    .view$output |>
      gt::tab_style(style = gt::cell_borders(style = NULL),
                    locations = list(gt::cells_column_labels(),
                                     gt::cells_stubhead(),
                                     gt::cells_stub(),
                                     gt::cells_body())) |>
      gt::data_color(columns = type,
                     target_columns = gt::everything(),
                     palette = palette)

  }

  if (!focus_na) {

    assign(glue::glue("{name}_view"), .view, envir = .GlobalEnv)

    easy_out(.view$output,
             filename = glue::glue("{name}_view"),
             width = width,
             ...)

  } else {

    .view$data |>
      dplyr::select(variable, label, na, na_prop) |>
      dplyr::filter(!is.na(na))

  }

}
