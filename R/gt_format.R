#' Title
#'
#' @param x
#' @param title
#' @param acro
#' @param note
#' @param note_p_ajust
#' @param vargrp
#' @param width
#' @param slide
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
gt_format <- \(x,
               title,
               acro = list(data, sep_ext = NULL),
               note = "",
               note_p_ajust = "",
               vargrp = list(label = "", note = ""),
               width = NULL,
               slide = FALSE,
               ...) {

### ACRO --------------------------------------------------------------------

  style <- x$table_styling$header$label

  body <-
  names(x$table_body) |>
    stringr::str_extract(".*label") |>
    stats::na.omit() |>
    purrr::map(~ x$table_body[[.]]) |>
    unlist()

  data_acro <- acro_extract(c(style, body, names(x)), acro$data)

### WIDTH & THEME -------------------------------------------------------------------

  if (!"gt_tbl" %in% class(x)) x <- gtsummary::as_gt(x)

  if (!is.null(width)) x <- x |> gt::tab_options(table.width = gt::px(width))

  x <- do.call("theme_gt", list(x, ...))

### TITLE & NOTES -------------------------------------------------------------------

  if (!slide) {

    x <- x |> gt::tab_header(gt::md(title))

    vars <- names(x[["_data"]])

    if (TRUE %in% stringr::str_starts(vars, "coef")) {

      data_acro <- data_acro[data_acro != "N"]

      x |>
        gt::tab_footnote(c(stringr::str_c(note),
                           str_acro(.estim$base,
                                    .estim$ajust,
                                    with(acro$data, mget(data_acro)),
                                    collapse = acro$sep_ext))) |>
        gt::tab_footnote(note_p_ajust,
                         gt::cells_column_labels(p.value_2))

    } else {

      x |>
        gt::tab_footnote(c(stringr::str_c(note),
                           str_acro(with(acro$data, mget(data_acro)),
                                    collapse = acro$sep_ext))) |>
        gt::tab_footnote(vargrp$note,
                         gt::cells_body(columns = label,
                                        rows = variable %in% vargrp$label))

    }

  } else x

}
