.xlsx_add_sheet <- \(
  x,
  sheet,
  data,
  max_width = 60,
  halign = "center",
  font_size = 8,
  header_color = "#E5E5E5",
  border_color = "#999999",
  border_type = "thin",
  color = NULL
) {
  withr::local_options(openxlsx2.maxWidth = max_width)

  color <- color[names(color) %in% names(data)]

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
        style = proto(if (nrow(data)) 2L else 1L)
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
    wb_add_font(
      dims = params$dims$data,
      size = font_size
    ) |>
    wb_add_fill(
      dims = params$dims$cols,
      color = params$colors$header
    ) |>
    wb_set_col_widths(
      cols = seq_len(ncol(data)),
      widths = "auto"
    ) |>
    wb_add_cell_style(
      dims = params$dims$full,
      horizontal = halign,
      vertical = "center",
      wrap_text = TRUE
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
#'       `"right"`. Default: `"center"`.
#'     \item `font_size`: Base font size for data cells. Header row uses
#'       `font_size + 1`. Default: `8`.
#'     \item `header_color`: Fill color for the header row (hex string).
#'       Default: `"#E5E5E5"`.
#'     \item `border_color`: Border color for all cells (hex string).
#'       Default: `"#999999"`.
#'     \item `border_type`: Border line type passed to
#'       [openxlsx2::wb_add_border()]. Default: `"thin"`.
#'     \item `color`: Named list mapping column names to hex colors for
#'       bold-colored data cells, e.g. `list(status = "#FF0000")`. `NULL`
#'       skips coloring.
#'   }
#'
#' @return An `openxlsx2` Workbook object.
#' @export
#'
#' @examples
#' wb <- get_xlsx(list(iris = iris, mtcars = mtcars))
#' # wb_save(wb, "output.xlsx")
get_xlsx <- \(sheets, ...) {
  if (!is_named(sheets)) {
    cli::cli_abort("{.arg sheets} must be a fully named list.")
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
