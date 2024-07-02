#' Title
#'
#' @param x
#' @param title
#' @param label_vargrp
#' @param note_vargrp
#' @param note_pvalue
#' @param note_global
#' @param acro_list
#' @param acro_sep
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
               label_vargrp = "",
               note_vargrp = "",
               note_pvalue = "",
               note_global = "",
               acro_list,
               acro_sep,
               width = NULL,
               slide = FALSE,
               ...) {

### ACRO --------------------------------------------------------------------

  style <- x$table_styling$header$label

  body <-
  names(x$table_body) |>
    str_extract(".*label") |>
    na.omit() |>
    map(~ x$table_body[[.]]) |>
    unlist()

  .acro <- acro_extract(c(style, body, names(x)), acro_list)

### WIDTH & THEME -------------------------------------------------------------------

  if (!"gt_tbl" %in% class(x)) x <- as_gt(x)

  if (!is.null(width)) x <- x |> tab_options(table.width = px(width))

  x <- do.call("theme_gt", list(x, ...))

### TITLE & NOTES -------------------------------------------------------------------

  if (!slide) {

    x <- x |> gt::tab_header(gt::md(title))

    if (TRUE %in% str_starts(names(x[["_data"]]), "coef")) {

      .acro <- .acro[.acro != "N"]

      x |>
        gt::tab_footnote(c(str_c(note_global),
                           acro_str(.estim$base,
                                    .estim$ajust,
                                    with(acro_list, mget(.acro)),
                                    collapse = acro_sep))) |>
        gt::tab_footnote(note_pvalue,
                         gt::cells_column_labels(p.value_2))

    } else {

      x |>
        gt::tab_footnote(c(str_c(note_global),
                           acro_str(with(acro_list, mget(.acro)),
                                    collapse = acro_sep))) |>
        gt::tab_footnote(note_vargrp,
                         gt::cells_body(columns = label,
                                        rows = variable %in% label_vargrp))

    }

  } else x

}
