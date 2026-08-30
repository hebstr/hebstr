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
#' @param title Optional table title, interpreted as markdown. On the Word
#'   branch it becomes the `flextable` caption, which the Quarto `docx` pipeline
#'   drops: caption a table rendered from Quarto through the chunk's `tbl-cap`
#'   option, which serves both formats and carries the cross-references.
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
#' @param width Table width, in pixels. Defaults to `700`. On the Word branch it
#'   is converted to the fraction of `page_width` that
#'   [flextable::set_table_properties()] expects, capped at `1`. Set to `NULL` to
#'   let the table keep its natural width.
#' @param page_width Usable text width, in inches, used to convert `width` on
#'   the Word branch, and read on that branch only. Defaults to the `page_width`
#'   option, itself unset: the width is then measured on the `reference-doc` of
#'   the document being rendered, looked up by [docx_page_width()] in the YAML
#'   front matter then in the Quarto extension providing the Word format. A
#'   document declaring neither falls back to `6.5`, a US Letter page with
#'   one-inch margins; one declaring a template that cannot be measured falls
#'   back too, with a warning. It is a property of the rendered document rather
#'   than of the table: where the lookup cannot reach it, set it once through
#'   [set_opts()] rather than at each call.
#' @param title_align Horizontal alignment of the title (`"justify"`, `"left"`,
#'   `"center"`, or `"right"`). Defaults to `"justify"`. Exposed here rather
#'   than left to `...` because the two themes default it differently
#'   ([theme_gt()] justifies, [theme_ft()] left-aligns): one value drives both
#'   branches, so a table titled from a single source is aligned the same way
#'   in HTML and in Word.
#' @param ... Passed on to [theme_gt()], or to [theme_ft()] under
#'   `options(hebstr.docx = TRUE)`. Set the table width through `width` and the
#'   title alignment through `title_align` rather than here: the two themes take
#'   the first in different units and default the second differently.
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
  width = 700,
  page_width = check_opts(page_width),
  title_align = "justify",
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

  .check_size(width, "width", allow_null = TRUE)

  # forcing the default would resolve the template on the gt branch too
  if (!missing(page_width)) {
    .check_size(page_width, "page_width", allow_null = FALSE)
  }

  clear_vars()

  if (collapse_missing && any(x$table_body$row_type %in% "missing")) {
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

  if (.is_docx()) {
    page_width <- page_width %||% .page_width()

    .check_size(page_width, "page_width", allow_null = FALSE)

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
      width = .page_fraction(width, page_width),
      title_align = title_align,
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
    theme_gt(width = width, title_align = title_align, ...)

  if (size_dm) {
    x <- tbl_font_size(x, columns = dm, size = missing_size)
  }

  ### FOOTNOTES ----------------------------------------------------------------

  notes <- c(str_c(note_global), .acro_str)

  if (length(notes) > 0) {
    x <- tab_footnote(x, footnote = str_flatten(notes, collapse = " "))
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
  width,
  title_align,
  ...
) {
  notes <- c(str_c(note_global), acro_note)

  if (length(notes) > 0) {
    x <- modify_source_note(x, str_flatten(notes, collapse = " "))
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

  theme_ft(
    .ft_render(.ft_breaks(x)),
    width = width,
    title_align = title_align,
    ...
  )
}

.mark_clash <- "'big\\.mark' and 'decimal\\.mark'"

.ft_render <- \(x) {
  withCallingHandlers(
    as_flex_table(x),
    # flextable renders footnote symbols through its own `big.mark`, a comma
    # that base R flags as ambiguous under a French `OutDec`
    warning = \(w) {
      if (str_detect(conditionMessage(w), .mark_clash)) {
        invokeRestart("muffleWarning")
      }
    }
  )
}

.br <- "<br\\s*/?>"

.bold_all <- "^\\*\\*[^*]*\\*\\*$"

.line_break <- \(label) {
  if_else(
    str_detect(label, .bold_all),
    # a header wrapped in one pair of markers loses its bold once split, so each
    # line carries its own pair
    str_replace_all(label, .br, "**\n**"),
    str_replace_all(label, .br, "\n")
  )
}

.ft_breaks <- \(x) {
  x$table_styling$header <- mutate(
    x$table_styling$header,
    label = .line_break(label)
  )

  if (!is.null(x$table_styling$spanning_header)) {
    x$table_styling$spanning_header <- mutate(
      x$table_styling$spanning_header,
      spanning_header = .line_break(spanning_header)
    )
  }

  x
}

.px_per_in <- 96

.check_size <- \(value, arg, allow_null) {
  if (allow_null && is.null(value)) {
    return(invisible(NULL))
  }

  if (!is_scalar_double(value) && !is_scalar_integer(value)) {
    cli_abort("{.arg {arg}} must be a single positive number.")
  }

  if (is.na(value) || value <= 0) {
    cli_abort("{.arg {arg}} must be a single positive number.")
  }

  invisible(NULL)
}

.page_fraction <- \(width, page_width) {
  if (is.null(width)) {
    return(NULL)
  }

  min(width / (page_width * .px_per_in), 1)
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
