.px_to_pt <- 0.75

.abort_tbl <- \(x, arg) {
  cli_abort(
    c(
      "{.arg {arg}} must be a {.cls gt_tbl} or a {.cls flextable} object, not {.obj_type_friendly {x}}.",
      i = "Table style verbs run on the output of {.fun tbl_format}."
    )
  )
}

#' Set the font size of table columns
#'
#' Applies a font size to the body cells of the targeted columns, on either a
#' [gt::gt()] or a [flextable::flextable()] object. `tbl_format()` returns a
#' `flextable` under `options(hebstr.docx = TRUE)` and a `gt_tbl` otherwise, so
#' this verb keeps a single call site across both output formats.
#'
#' @param x A `gt_tbl` or a `flextable` object, typically from [tbl_format()].
#' @param columns Columns to style, using <[`tidy-select`][dplyr::select]>
#'   syntax.
#' @param size Font size, in pixels. Converted to points (`size * 0.75`) on the
#'   `flextable` branch, whose native unit is the point.
#'
#' @return An object of the same class as `x`.
#' @export
#'
#' @examples
#' tbl <- gt::gt(head(penguins)) |>
#'   tbl_font_size(columns = species, size = 11)
#'
tbl_font_size <- \(x, columns, size) {
  if (!is_scalar_double(size) && !is_scalar_integer(size)) {
    cli_abort("{.arg size} must be a single number.")
  }

  .columns <- enquo(columns)

  if (inherits(x, "gt_tbl")) {
    return(tab_style(
      x,
      style = cell_text(size = px(size)),
      locations = cells_body(columns = !!.columns)
    ))
  }

  if (inherits(x, "flextable")) {
    .j <- names(select(x$body$dataset, !!.columns))

    return(fontsize(x, size = size * .px_to_pt, j = .j, part = "body"))
  }

  .abort_tbl(x, "x")
}

#' Set the text color of table rows
#'
#' Applies a text color to the body rows matching a predicate, on either a
#' [gt::gt()] or a [flextable::flextable()] object. Companion to
#' [tbl_font_size()]; see there for the dual-branch rationale.
#'
#' The predicate is evaluated against the table body. On the `flextable` branch
#' that body holds the visible columns only, so a predicate targeting a column
#' hidden by `tbl_format()` (such as `variable` or `row_type`) resolves on the
#' `gt_tbl` branch and fails on the `flextable` one.
#'
#' @param x A `gt_tbl` or a `flextable` object, typically from [tbl_format()].
#' @param rows A predicate expression evaluated against the table body, used to
#'   select the rows to style.
#' @param color Text color, as a hex string or a color name.
#'
#' @return An object of the same class as `x`.
#' @export
#'
#' @examples
#' tbl <- gt::gt(head(penguins)) |>
#'   tbl_row_color(rows = species == "Adelie", color = "#B22222")
#'
tbl_row_color <- \(x, rows, color) {
  if (!is_scalar_character(color)) {
    cli_abort("{.arg color} must be a single string.")
  }

  .rows <- enquo(rows)

  if (inherits(x, "gt_tbl")) {
    return(tab_style(
      x,
      style = cell_text(color = color),
      locations = cells_body(rows = !!.rows)
    ))
  }

  if (inherits(x, "flextable")) {
    .i <- eval_tidy(.rows, data = x$body$dataset)

    if (!any(.i, na.rm = TRUE)) {
      return(x)
    }

    return(flextable::color(
      x,
      color = color,
      i = which(.i %in% TRUE),
      part = "body"
    ))
  }

  .abort_tbl(x, "x")
}
