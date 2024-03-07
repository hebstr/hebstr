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
str_acro <- \(..., collapse = NULL) {

  acro <- stringr::str_c(c(...), collapse = collapse)
  glue::glue("{acro}.")

}


#' Title
#'
#' @param title
#' @param note
#' @param acro
#' @param sub_size
#'
#' @return
#' @export
#'
#' @examples
#'
str_fig <- \(title,
             note = "",
             acro = "",
             sub_size = 7.5) {

  title <- glue::glue(title)
  note <- glue::glue(note)

  glue::glue("{title}<br>
             <span style='font-size:{sub_size}pt'>
             {note} {acro}.
             </span>")

}
