.xlsx_align <- "center"

# openxlsx2 measures a column in characters of the workbook's base font, so a
# sheet set in a smaller face comes out wider than its text; the header carries
# its autofilter button on top, which its own length does not account for
.xlsx_widths <- \(wb, data, font_size, header_size, filter = 3, pad = 1) {
  base <- as.numeric(wb_get_base_font(wb)$size$val)
  em <- \(chars, size) chars * size / base

  imap_dbl(data, \(col, name) {
    values <- as.character(col)
    values <- values[!is.na(values)]

    pad +
      max(
        em(nchar(name) + filter, header_size),
        em(if (length(values)) max(nchar(values)) else 0L, font_size)
      )
  })
}

.xlsx_add_sheet <- \(
  x,
  sheet,
  data,
  max_width = 60,
  halign = .xlsx_align,
  font_size = 8,
  header_color = "#E5E5E5",
  border_color = "#C5C5C5",
  border_type = "thin",
  color = NULL
) {
  withr::local_options(openxlsx2.maxWidth = max_width)

  # wb_dims(select = "data") collapses onto the header row on a zero-row frame,
  # so every data-scoped style has to be gated or it repaints the header
  has_data <- nrow(data) > 0L

  color <- if (has_data) color[names(color) %in% names(data)]

  # a named list aligns the columns it names and leaves the rest at the default
  by_col <- if (is.list(halign)) halign[names(halign) %in% names(data)]
  halign <- if (is.null(by_col)) halign else .xlsx_align

  params <- list(
    dims = list(
      full = wb_dims(x = data),
      data = wb_dims(x = data, select = "data"),
      cols = wb_dims(x = data, select = "col_names"),
      proto = wb_dims(
        rows = seq_len(min(nrow(data) + 1L, 2L)),
        cols = seq_len(ncol(data))
      )
    ),
    colors = list(
      border = wb_color(border_color),
      header = wb_color(header_color)
    )
  )

  add_data_font <- \(wb) {
    if (!has_data) {
      return(wb)
    }

    wb_add_font(
      wb = wb,
      dims = params$dims$data,
      size = font_size
    )
  }

  # posed before the border, so that the prototype row spread_style() broadcasts
  # already carries the column's own alignment
  add_halign <- \(wb, col, align) {
    wb_add_cell_style(
      wb = wb,
      dims = wb_dims(
        rows = seq_len(nrow(data) + 1L),
        cols = match(col, names(data))
      ),
      horizontal = align,
      vertical = "center",
      wrap_text = TRUE
    )
  }

  add_color <- \(wb, vars, color) {
    wb_add_font(
      wb = wb,
      dims = wb_dims(x = data, cols = vars, select = "data"),
      color = wb_color(color),
      size = font_size,
      bold = TRUE
    )
  }

  # openxlsx2 scales quadratically on a wide border range, so the border is
  # resolved on the two prototype rows then broadcast; a cell style carries the
  # column's number format, hence one prototype per column and not one per sheet
  spread_style <- \(wb, col) {
    proto <- \(row) {
      wb_get_cell_style(wb, dims = wb_dims(rows = row, cols = col))
    }

    wb |>
      wb_set_cell_style(
        dims = wb_dims(x = data, cols = col, select = "col_names"),
        style = proto(1L)
      ) |>
      wb_set_cell_style(
        dims = wb_dims(x = data, cols = col, select = "data"),
        style = proto(if (has_data) 2L else 1L)
      )
  }

  wb_add_worksheet(
    wb = x,
    sheet = sheet,
    zoom = 105
  ) |>
    wb_add_data_table(
      x = data,
      na.strings = NULL
    ) |>
    wb_add_font(
      dims = params$dims$cols,
      size = font_size + 1,
      bold = TRUE
    ) |>
    add_data_font() |>
    wb_add_fill(
      dims = params$dims$cols,
      color = params$colors$header
    ) |>
    wb_set_col_widths(
      cols = seq_len(ncol(data)),
      widths = .xlsx_widths(x, data, font_size, font_size + 1)
    ) |>
    wb_add_cell_style(
      dims = params$dims$full,
      horizontal = halign,
      vertical = "center",
      wrap_text = TRUE
    ) |>
    reduce2(
      .x = names(by_col),
      .y = by_col,
      .f = add_halign,
      .init = _
    ) |>
    wb_add_border(
      dims = params$dims$proto,
      top_color = params$colors$border,
      top_border = border_type,
      bottom_color = params$colors$border,
      bottom_border = border_type,
      left_color = params$colors$border,
      left_border = border_type,
      right_color = params$colors$border,
      right_border = border_type,
      inner_hcolor = params$colors$border,
      inner_hgrid = border_type,
      inner_vcolor = params$colors$border,
      inner_vgrid = border_type
    ) |>
    reduce(
      .x = seq_len(ncol(data)),
      .f = spread_style,
      .init = _
    ) |>
    reduce2(
      .x = names(color),
      .y = color,
      .f = add_color,
      .init = _
    )
}

#' Build a styled multi-sheet Excel workbook
#'
#' Takes a named list of data frames and produces an `openxlsx2` workbook with
#' one formatted sheet per element.
#'
#' @param sheets Named list of data frames. Each name becomes a sheet name.
#' @param ... Styling arguments forwarded to the internal sheet builder:
#'   \itemize{
#'     \item `max_width`: Maximum column width forwarded to
#'       `openxlsx2.maxWidth`. Default: `60`.
#'     \item `halign`: Horizontal cell alignment. One of `"center"`, `"left"`,
#'       `"right"`, or a named list aligning the columns it names and leaving
#'       the others at the default, e.g. `list(variable = "left")`. Default:
#'       `"center"`.
#'     \item `font_size`: Base font size for data cells. Header row uses
#'       `font_size + 1`. Default: `8`.
#'     \item `header_color`: Fill color for the header row (hex string).
#'       Default: `"#E5E5E5"`.
#'     \item `border_color`: Border color for all cells (hex string).
#'       Default: `"#C5C5C5"`.
#'     \item `border_type`: Border line type passed to
#'       [openxlsx2::wb_add_border()]. Default: `"thin"`.
#'     \item `color`: Named list mapping column names to hex colors for
#'       bold-colored data cells, e.g. `list(status = "#FF0000")`. `NULL`
#'       skips coloring.
#'   }
#'
#' @return An `openxlsx2` Workbook object. Write it to disk with [easy_out()].
#'
#' @seealso [easy_out()], which saves the workbook as an XLSX file.
#' @export
#'
#' @examples
#' wb <- get_xlsx(list(iris = iris, mtcars = mtcars))
#'
#' \dontrun{
#' easy_out(wb, filename = "tables")
#' }
#'
get_xlsx <- \(sheets, ...) {
  if (!is_named(sheets)) {
    cli::cli_abort("{.arg sheets} must be a fully named list.")
  }

  color <- list(...)$color

  if (length(color) && !is_named(color)) {
    cli::cli_abort("{.arg color} must be a fully named list.")
  }

  halign <- list(...)$halign

  if (is.list(halign) && !is_named(halign)) {
    cli::cli_abort("{.arg halign} must be a fully named list.")
  }

  reduce2(
    .x = sheets,
    .y = names(sheets),
    .f = \(wb, data, name) {
      .xlsx_add_sheet(
        x = wb,
        sheet = name,
        data = data,
        ...
      )
    },
    .init = wb_workbook()
  )
}
