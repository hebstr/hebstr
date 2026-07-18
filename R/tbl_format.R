#' Finalise a gtsummary table into a styled table
#'
#' Converts a `gtsummary` table into a themed table: applies the package
#' theme, an optional markdown title, automatic acronym footnotes drawn
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
#' @param collapse_missing Logical scalar. When `TRUE` (the default), the
#'   automatic missing-value rows are folded into a single `dm` column via
#'   [col_missing()] and that column is set to `missing_size`. Tables built with
#'   `gtsummary::tbl_summary(missing = "no")`, or carrying no missing rows, are
#'   left untouched.
#' @param missing_size Font size, in pixels, applied to the `dm` column when
#'   `collapse_missing = TRUE`. Defaults to `11`.
#' @param ... Passed on to [theme_gt()], or to [theme_ft()] under
#'   `options(hebstr.docx = TRUE)`.
#'
#' @returns A `gt_tbl` object, or a `flextable` object under
#'   `options(hebstr.docx = TRUE)`. Style verbs applied downstream must handle
#'   both classes: see [tbl_font_size()] and [tbl_row_color()].
#'
#' @examples
#' set_opts()
#' tbl <- gtsummary::tbl_summary(mtcars, include = c(mpg, cyl))
#' tbl_format(tbl)
#'
#' @export
#'
tbl_format <- \(
  x,
  title = NULL,
  note_global = NULL,
  note_pvalue = NULL,
  label_vargrp = NULL,
  note_vargrp = NULL,
  acro_list = check_opts(acro),
  acro_sep = check_opts(sep$ext),
  zero_replace = "^0\\s",
  collapse_missing = TRUE,
  missing_size = 11,
  ...
) {
  if (!inherits(x, "gtsummary")) {
    cli_abort(
      c(
        "{.arg x} must be a {.cls gtsummary} table.",
        i = "Build it with {.fun gtsummary::tbl_summary}, {.fun gtsummary::tbl_regression} (optionally via {.fun gtsum_format}) before {.fun tbl_format}."
      )
    )
  }

  if (!is_bool(collapse_missing)) {
    cli_abort(
      "{.arg collapse_missing} must be a single {.code TRUE} or {.code FALSE}."
    )
  }

  clear_vars()

  if (collapse_missing && any(x$table_body$row_type == "missing")) {
    x <- col_missing(x)
  }

  size_dm <- collapse_missing && "dm" %in% names(x$table_body)

  ### ACRONYMS ----------------------------------------------------------------

  style <- c(
    x$table_styling$header$label,
    x$table_styling$spanning_header$spanning_header
  )

  body <- x$table_body |>
    names() |>
    str_subset("label") |>
    map(~ x$table_body[[.x]]) |>
    unlist()

  .acro <- acro_extract(c(style, body), acro_list)

  x <- x$table_styling$abbreviation |>
    distinct(abbreviation) |>
    pull() |>
    reduce(remove_abbreviation, .init = x)

  ### ESTIMATOR ----------------------------------------------------------------

  is_coef <- any(str_starts(names(x$table_body), "coef"))

  if (is_coef) {
    if (!exists("estim", envir = .estim_channel, inherits = FALSE)) {
      cli_abort(
        c(
          "No estimator annotation is available for this coefficient table.",
          i = "Run {.fun gtsum_format} on the regression table, then {.fun tbl_format}, in the same session."
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

  p_col <- str_subset(names(x$table_body), "^p\\.value")

  if (is_coef && !is.null(note_pvalue) && length(p_col) == 0) {
    cli_abort(
      c(
        "No p-value column to attach {.arg note_pvalue} to.",
        i = "This coefficient table has no {.field p.value} column."
      )
    )
  }

  if (!is_coef && !is.null(note_vargrp) && is.null(label_vargrp)) {
    cli_abort(
      c(
        "{.arg note_vargrp} needs {.arg label_vargrp} to anchor the footnote.",
        i = "Name the targeted variable(s) in {.arg label_vargrp}."
      )
    )
  }

  ### RENDER -------------------------------------------------------------------

  if (getOption("hebstr.docx", default = FALSE)) {
    x <- .fmt_ft(
      x,
      title = title,
      note_global = note_global,
      note_pvalue = note_pvalue,
      label_vargrp = label_vargrp,
      note_vargrp = note_vargrp,
      acro_note = .acro_str,
      is_coef = is_coef,
      zero_replace = zero_replace,
      ...
    )

    if (size_dm) {
      x <- tbl_font_size(x, columns = dm, size = missing_size)
    }

    return(x)
  }

  if (!inherits(x, "gt_tbl")) {
    x <- as_gt(x)
  }

  x <- x |>
    tab_header(title = if (!is.null(title)) md(title)) |>
    theme_gt(...)

  if (size_dm) {
    x <- tbl_font_size(x, columns = dm, size = missing_size)
  }

  ### FOOTNOTES ----------------------------------------------------------------

  if (!is.null(note_global) || !is.null(.acro_str)) {
    x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))
  }

  if (is_coef && !is.null(note_pvalue)) {
    x <- tab_footnote(
      x,
      footnote = note_pvalue,
      locations = cells_column_labels(all_of(tail(p_col, 1)))
    )
  }

  if (!is_coef && !is.null(note_vargrp)) {
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


.fmt_ft <- \(
  x,
  title,
  note_global,
  note_pvalue,
  label_vargrp,
  note_vargrp,
  acro_note,
  is_coef,
  zero_replace,
  ...
) {
  notes <- c(str_c(note_global), acro_note)

  if (length(notes) > 0) {
    x <- reduce(notes, modify_source_note, .init = x)
  }

  if (is_coef && !is.null(note_pvalue)) {
    x <- modify_footnote_header(
      x,
      footnote = note_pvalue,
      columns = tail(str_subset(names(x$table_body), "^p\\.value"), 1)
    )
  }

  if (!is_coef && !is.null(note_vargrp)) {
    x <- modify_footnote_body(
      x,
      footnote = note_vargrp,
      columns = "label",
      rows = inject(variable %in% !!label_vargrp)
    )
  }

  if (!is.null(zero_replace)) {
    shown <- x$table_styling$header$column[!x$table_styling$header$hide]

    x <- modify_table_body(
      x,
      \(.b) {
        mutate(
          .b,
          across(
            all_of(intersect(shown, names(.b))) & where(is.character),
            \(.c) if_else(str_detect(.c, zero_replace), "0", .c, missing = .c)
          )
        )
      }
    )
  }

  if (!is.null(title)) {
    x <- modify_caption(x, title)
  }

  theme_ft(as_flex_table(x), ...)
}

#' Finalise a gtsummary table into a styled gt table
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' Renamed to [tbl_format()]: the function returns a `flextable` under
#' `options(hebstr.docx = TRUE)`, which the `gt_` prefix contradicts.
#'
#' @param ... Passed on to [tbl_format()].
#'
#' @returns A `gt_tbl` object, or a `flextable` object under
#'   `options(hebstr.docx = TRUE)`.
#'
#' @keywords internal
#' @export
#'
gt_format <- \(...) {
  deprecate_soft("0.0.0.9000", "gt_format()", "tbl_format()")

  tbl_format(...)
}
