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


#' Title
#'
#' @param text
#' @param color
#' @param bg
#'
#' @return
#' @export
#'
#' @examples
#'
str_color <- \(text,
               color = "red",
               bg = "#ffffff00") {

color <- glue::glue("color:{color}")
bg <- glue::glue("background-color:{bg}")

htmltools::tags$span(glue::glue("**{text}**"),
                     style = glue::glue("{color};{bg}"))

}
