#' Title
#'
#' @param data
#' @param rowname_col
#' @param groupname_col
#' @param ...
#' @param title
#' @param width
#' @param font
#' @param digit
#' @param palette
#' @param arrange
#'
#' @return
#' @export
#'
#' @examples
gt_heatmap <- \(data,
                rowname_col = NULL,
                groupname_col = NULL,
                ...,
                title = NULL,
                width = NULL,
                font = "arial",
                digit = 1,
                palette = c("indianred2", "skyblue1"),
                arrange = FALSE) {

if (arrange) {

data <-
data |>
  dplyr::rowwise() |>
    dplyr::mutate(.sum = sum(dplyr::across(dplyr::where(is.numeric)))) |>
    dplyr::ungroup() |>
  dplyr::arrange(dplyr::desc(.sum)) |>
  dplyr::select(-.sum)

}

limit <-
data |>
  tidyr::pivot_longer(cols = dplyr::where(is.numeric)) |>
  dplyr::pull(value) |>
  max()

data |>
  gt::gt(rowname_col = rowname_col,
         groupname_col = groupname_col) |>
  gt::tab_header(if (!is.null(title)) gt::md(title) else title) |>
  gt::opt_align_table_header(align = "left") |>
  gt::opt_table_font(font = font) |>
  gt::tab_options(table.width = if (!is.null(width)) gt::px(width) else width,
                  table.font.size = gt::px(12),
                  heading.title.font.size = gt::pct(95),
                  heading.border.bottom.style = "none",
                  table_body.hlines.style = "none",
                  table.border.top.style = "none",
                  table.border.bottom.style = "none",
                  table_body.border.top.style = "none",
                  table_body.border.bottom.style = "none",
                  column_labels.border.bottom.style = "none",
                  column_labels.border.top.style = "none",
                  stub.border.style = "none",
                  ...) |>
  gt::tab_style(style = gt::cell_text(align = "center",
                                      weight = "bold"),
                locations = list(gt::cells_column_labels())) |>
  gt::tab_style(style = gt::cell_text(align = "right",
                                      weight = "bold"),
                locations = list(gt::cells_stub())) |>
  gt::tab_style(style = gt::cell_text(align = "center"),
                locations = list(gt::cells_body())) |>
  gt::fmt_number(columns = dplyr::where(is.numeric),
                 decimals = digit) %>%
  gt::data_color(method = "numeric",
                 palette = c(palette[1], "white", palette[2]),
                 domain = c(-limit, limit))

}
