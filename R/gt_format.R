#' Title
#'
#' @param x arg
#' @param title arg
#' @param note_global arg
#' @param note_pvalue arg
#' @param label_vargrp arg
#' @param note_vargrp arg
#' @param acro_list arg
#' @param acro_sep arg
#' @param zero_replace arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
gt_format <- \(
  x,
  title = NULL,
  note_global = NULL,
  note_pvalue = NULL,
  label_vargrp = NULL,
  note_vargrp = NULL,
  acro_list = check_opts(acro),
  acro_sep = check_opts(sep$ext),
  zero_replace = "^0\\s",
  ...
) {

  clear_vars()

### ACRO --------------------------------------------------------------------

  style <- with(
    data = x$table_styling,
    expr = c(header$label, spanning_header$spanning_header)
  )

  body <-
  x$table_body |>
    names() |>
    str_subset(".*label") |>
    map(~ x$table_body[[.]]) |>
    unlist()

  .acro <- acro_extract(c(style, body, names(x)), acro_list)

  x <-
  x$table_styling$abbreviation |>
    distinct(abbreviation) |>
    pull() |>
    reduce(remove_abbreviation, .init = x)

### THEME -------------------------------------------------------------------

  if (!"gt_tbl" %in% class(x)) x <- as_gt(x)

  x <-
  x |>
    tab_header(title = if (!is.null(title)) md(title)) |>
    theme_gt(...)

  if (TRUE %in% str_starts(names(x[["_data"]]), "coef")) {

    .acro_str <- acro_str(
      .estim$uv,
      .estim$mv,
      with(acro_list, mget(.acro[.acro != "N"])),
      collapse = acro_sep
    )

    if (!is.null(note_global) || !is.null(.acro_str)) {

      x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))

    }

    if (!is.null(note_pvalue)) {

      x <- tab_footnote(
        x,
        footnote = note_pvalue,
        locations = cells_column_labels(p.value_2)
      )

    }

  } else {

    .acro_str <- acro_str(
      with(acro_list, mget(.acro)),
      collapse = acro_sep
    )

    if (!is.null(note_global) || !is.null(.acro_str)) {

      x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))

    }

    if (!is.null(note_vargrp)) {

      x <- tab_footnote(
        x,
        footnote = note_vargrp,
        locations = cells_body(
          columns = label,
          rows = variable %in% label_vargrp
        )
      )

    }

  }

  if (!is.null(zero_replace)) {

    x <- sub_values(x, pattern = zero_replace, replacement = 0)

  }

  return(x)

}
