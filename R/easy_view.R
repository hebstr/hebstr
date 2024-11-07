#' Title
#'
#' @param x 
#' @param name 
#' @param width 
#' @param font_size 
#' @param font_family 
#' @param color_type 
#' @param palette 
#' @param out 
#' @param ... 
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
               font_family = check_fonts(.auto = "luciole"),
               color_type = FALSE,
               palette = "viridis",
               raw = FALSE,
               out = FALSE,
               ...) {

  if (is.null(name)) name <- enexpr(x)

  set_cols <- \(y, vars, fn, name) {

    vars <- enexprs(vars)
    fn <- enexpr(fn)

    data <-
    y |>
      select(!!!vars) |>
      names() |>
      set_names() |>
      map(~ eval(fn))

    data <-
    tibble(variable = names(data),
           !!name := data)

  }

  .cols <-
  list(range =
         set_cols(x,
                  vars = where(is.numeric) | where(is.Date),
                  fn = range(x[[.]], na.rm = TRUE) |> round(1),
                  name = "range"),
       q1_q2_q3 =
         set_cols(x,
                  vars = where(is.numeric),
                  fn = quantile(x[[.]],
                                probs = c(.25, .5, .75),
                                na.rm = TRUE) |> round(1),
                  name = "q1_q2_q3"),
       bin =
         set_cols(x,
                  vars = where(is.numeric),
                  fn = length(unique(na.omit(x[[.]]))) == 2,
                  name = "bin"))

  .view <-
  lst(data =
        .cols |>
          reduce(left_join,
                 by = join_by(variable),
                 .init = generate_dictionary(x) |> tibble()) |>
          rename(type = col_type,
                 n_miss = missing) |>
          mutate(type = case_when(bin == "TRUE" ~ "bin",
                                  bin == "FALSE" ~ "num",
                                  .default = type),
                 n_miss = na_if(n_miss, 0),
                 q1_q2_q3 = if_else(type == "bin", NA, q1_q2_q3)) |>
          mutate(p_miss = percent(n_miss / nrow(x), accuracy = .1),
                 .after = n_miss) |>
          relocate(range, q1_q2_q3, .before = levels) |>
          select(where(~ !is.null(unlist(.))), -bin),
      output =
        data |>
          gt(rowname_col = "pos") |>
          sub_missing(missing_text = "") |>
          tab_options(data_row.padding = 2,
                      table.width = px(width),
                      table.font.size = px(font_size)) |>
          tab_style(style = cell_text(weight = "bold"),
                    locations = cells_column_labels()) |>
          opt_table_font(font = font_family))

  if (color_type) {

    .view$output <-
    .view$output |>
      tab_style(style = cell_borders(style = NULL),
                locations = list(cells_column_labels(),
                                 cells_stubhead(),
                                 cells_stub(),
                                 cells_body())) |>
      data_color(columns = type,
                 target_columns = gt::everything(),
                 palette = palette)

  }

  if (!raw) {
  
    assign(glue("{name}_view"), .view, envir = .GlobalEnv)
  
    if (out) {
      
      easy_out(.view$output,
               filename = glue("{name}_view"),
               width = width,
               assign = FALSE,
               ...)
      
    } else .view$output
    
  } else .view$data

}
