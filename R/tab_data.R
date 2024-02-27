#' Extract metadata from a gt/gtsummary object.
#'
#' @param x Data input. A gt or gtsummary object.
#'
#' @return One or more tibbles
#' @export
#'
#' @examples
#'
tab_data <- \(x) {

  class <- class(x)[1]
  y <- substitute(x)

  if (class != "gt_tbl") {

  .data <-
  assign(glue::glue("{y}_meta"),
         x$meta_data |>
           tidyr::unnest(cols = "df_stats",
                         names_repair = "unique"),
         envir = .GlobalEnv) |>
    dplyr::expr()

  .meta_data <-
  list(assign(glue::glue("{y}_meta"),
              x$meta_data,
              envir = .GlobalEnv),
       assign(glue::glue("{y}_data"),
              x$inputs$data %>%
                dplyr::filter(.[[2]] != 0),
              envir = .GlobalEnv)) |>
    dplyr::expr()

  .body_style <-
  list(assign(glue::glue("{y}_body"),
              x$table_body,
              envir = .GlobalEnv),
       assign(glue::glue("{y}_style"),
              x$table_styling$header,
              envir = .GlobalEnv)) |>
    dplyr::expr()

  class |>
    switch("tbl_summary" = eval(.data),
           "tbl_uvregression" = eval(.meta_data),
           "tbl_regression" = eval(.body_style),
           "tbl_merge" = eval(.body_style),
           "tbl_strata" = eval(.body_style))

  } else {

  assign(glue::glue("{y}.gt_style"),
         x$`_options`,
         envir = .GlobalEnv)

  }

}
