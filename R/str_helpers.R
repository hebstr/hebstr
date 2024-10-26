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

  stringr::str_c(c(...), collapse = "|")

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

  color <- glue("color:{color};")
  bg <- glue("background-color:{bg};")

  glue("<span style='{color}{bg}'>**{text}**</span>")

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

  title <- glue(title)
  note <- glue(note)

  glue("{title}<br>
       <span style='font-size:{sub_size}pt'>
       {note} {acro}
       </span>")

}


#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
str_label <- \(data, ..., last = "and") {

  c(...) |>
    map_chr(~ with(data, get(.)) |>
              labelled::var_label()) |>
    stringr::str_flatten_comma(glue::glue(" {last} "))

}


#' Title
#'
#' @param .f
#' @param str
#'
#' @return
#' @export
#'
#' @examples
#'
str_cap <- \(fun, str) {

  cap <- stringr::str_sub(str, end = 1)
  call <- do.call(fun, list(cap))

  stringr::str_replace(str, cap, call)

}
