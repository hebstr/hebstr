#' Title
#'
#' @param ...
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
#'
acro <- \(..., sep) {

  e <- rlang::env("~" = \(x, y) glue::glue("{rlang::enexpr(x)}{sep}{y}"))

  list(...) |>
    purrr::map(~ eval(rlang::enexpr(.), e)) %>%
    rlang::set_names(stringr::str_extract(., "\\w+"))

}


#' Title
#'
#' @param x
#' @param acro_list
#'
#' @return
#' @export
#'
#' @examples
#'
acro_extract <- \(x, acro_list) {

  .str <- stringr::str_c(names(acro_list), collapse = "\\b|\\b")

  x |>
    stringr::str_extract(glue::glue("\\b{.str}\\b")) |>
    na.omit() |>
    unique()

}


#' Title
#'
#' @param x
#' @param collapse
#'
#' @return
#' @export
#'
#' @examples
#'
acro_str <- \(..., collapse = NULL) {

  acro <- stringr::str_c(c(...), collapse = collapse)
  glue::glue("{acro}")

}


#' Title
#'
#' @param x
#' @param vars
#' @param acro_list
#' @param acro_sep
#'
#' @return
#' @export
#'
#' @examples
#'
acro_match <- \(x,
                vars = names(x),
                acro_list,
                acro_sep) {

.acro <-
vars |>
  map(~ with(x, get(.)) |>
        labelled::label_attribute()) |>
  unlist() |>
  acro_extract(acro_list)

acro_str(with(acro_list, mget(.acro)),
         collapse = acro_sep)

}
