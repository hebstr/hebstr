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
#' @param vars
#' @param acro_list
#' @param sep_ext
#'
#' @return
#' @export
#'
#' @examples
#'
match_acro <- \(x,
                vars = names(x),
                acro_list,
                sep_ext) {

acro <-
vars |>
  map(~ with(x, get(.)) |>
        labelled::label_attribute()) |>
  unlist() |>
  acro_extract(acro_list)

str_acro(with(acro_list, mget(acro)),
         collapse = sep_ext)

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
    str_flatten_comma(glue::glue(" {last} "))

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
