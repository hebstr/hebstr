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

  .str <-
  acro_list |> 
    names() |>
    str_c(collapse = "\\b|\\b")

  x |>
    str_extract(glue("\\b{.str}\\b")) |>
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
acro_str <- \(..., 
              collapse = NULL) {

  acro <- str_c(c(...), collapse = collapse)
  
  if (acro != "") glue("{acro}.") else NULL

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
        label_attribute()) |>
  unlist() |>
  acro_extract(acro_list)

acro_str(with(acro_list, mget(.acro)),
         collapse = acro_sep)

}
