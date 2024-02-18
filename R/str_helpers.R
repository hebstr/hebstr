#' Title
#'
#' @param ... One or more character vector
#'
#' @return A character vector
#' @export
#'
#' @examples
#'
str_u <- \(...) {

  stringr::str_c(unlist(c(...)), collapse = "|")

}
