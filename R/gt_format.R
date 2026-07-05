#' Finalise a gtsummary table into a styled gt table
#'
#' Converts a `gtsummary` table into a themed `gt` table: applies the package
#' `gt` theme, an optional markdown title, automatic acronym footnotes drawn
#' from a reference dictionary, optional user footnotes (global, p-value column,
#' variable group), and an optional zero-value substitution. Acronyms present in
#' the table headers, labels, and column names are detected and their
#' definitions appended as a footnote, replacing the native `gtsummary`
#' abbreviations.
#'
#' @param x A `gtsummary` table, typically built with
#'   [gtsummary::tbl_summary()] or [gtsummary::tbl_regression()], optionally
#'   pre-formatted with [gtsum_format()].
#' @param title Optional table title, interpreted as markdown.
#' @param note_global Optional character string added as a global table
#'   footnote, alongside the automatic acronym definitions.
#' @param note_pvalue Optional footnote attached to the p-value column.
#'   Coefficient (regression) tables only.
#' @param label_vargrp Character vector of variable names identifying the row
#'   group(s) targeted by `note_vargrp`. Non-coefficient tables only.
#' @param note_vargrp Optional footnote attached to the label cells of the
#'   variables named in `label_vargrp`. Non-coefficient tables only.
#' @param acro_list Named list used as the acronym dictionary: names are the
#'   acronyms to detect, values their definitions. Defaults to the `acro`
#'   option (`check_opts(acro)`).
#' @param acro_sep Separator inserted between acronym definitions in the
#'   footnote. Defaults to the `sep$ext` option.
#' @param zero_replace Regular expression; matching cell values are replaced
#'   with `0`. Set to `NULL` to disable. Defaults to `"^0\\s"`.
#' @param ... Passed on to [theme_gt()].
#'
#' @returns A `gt_tbl` object.
#'
#' @examples
#' set_opts()
#' tbl <- gtsummary::tbl_summary(mtcars, include = c(mpg, cyl))
#' gt_format(tbl)
#'
#' @export
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
  if (!inherits(x, "gtsummary")) {
    cli_abort(
      c(
        "{.arg x} must be a {.cls gtsummary} table.",
        i = "Build it with {.fun gtsummary::tbl_summary}, {.fun gtsummary::tbl_regression} (optionally via {.fun gtsum_format}) before {.fun gt_format}."
      )
    )
  }

  clear_vars()

  ### ACRONYMS ----------------------------------------------------------------

  style <- c(
    x$table_styling$header$label,
    x$table_styling$spanning_header$spanning_header
  )

  body <-
    x$table_body |>
    names() |>
    str_subset("label") |>
    map(~ x$table_body[[.x]]) |>
    unlist()

  .acro <- acro_extract(c(style, body), acro_list)

  x <-
    x$table_styling$abbreviation |>
    distinct(abbreviation) |>
    pull() |>
    reduce(remove_abbreviation, .init = x)

  ### THEME --------------------------------------------------------------------

  if (!inherits(x, "gt_tbl")) {
    x <- as_gt(x)
  }

  x <-
    x |>
    tab_header(title = if (!is.null(title)) md(title)) |>
    theme_gt(...)

  ### FOOTNOTES ----------------------------------------------------------------

  is_coef <- any(str_starts(names(x[["_data"]]), "coef"))

  if (is_coef) {
    if (!exists("estim", envir = .estim_channel, inherits = FALSE)) {
      cli_abort(
        c(
          "No estimator annotation is available for this coefficient table.",
          i = "Run {.fun gtsum_format} on the regression table, then {.fun gt_format}, in the same session."
        )
      )
    }

    .estim <- .estim_channel$estim

    .acro_str <- acro_str(
      .estim$uv,
      .estim$mv,
      acro_list[.acro[.acro != "N"]],
      collapse = acro_sep
    )
  } else {
    .acro_str <- acro_str(acro_list[.acro], collapse = acro_sep)
  }

  if (!is.null(note_global) || !is.null(.acro_str)) {
    x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))
  }

  if (is_coef && !is.null(note_pvalue)) {
    p_col <- str_subset(names(x[["_data"]]), "^p\\.value")

    if (length(p_col) == 0) {
      cli_abort(
        c(
          "No p-value column to attach {.arg note_pvalue} to.",
          i = "This coefficient table has no {.field p.value} column."
        )
      )
    }

    x <- tab_footnote(
      x,
      footnote = note_pvalue,
      locations = cells_column_labels(all_of(tail(p_col, 1)))
    )
  }

  if (!is_coef && !is.null(note_vargrp)) {
    if (is.null(label_vargrp)) {
      cli_abort(
        c(
          "{.arg note_vargrp} needs {.arg label_vargrp} to anchor the footnote.",
          i = "Name the targeted variable(s) in {.arg label_vargrp}."
        )
      )
    }

    x <- tab_footnote(
      x,
      footnote = note_vargrp,
      locations = cells_body(
        columns = label,
        rows = variable %in% label_vargrp
      )
    )
  }

  if (!is.null(zero_replace)) {
    x <- sub_values(x, pattern = zero_replace, replacement = 0)
  }

  x
}
