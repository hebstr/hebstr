#' Title
#'
#' @param data
#' @param rowname_col
#' @param groupname_col
#' @param ...
#' @param title
#' @param width
#' @param digit
#' @param font_family
#' @param font_size
#' @param color
#' @param palette
#' @param arrange
#'
#' @return
#' @export
#'
#' @examples
#'
gt_heatmap <- \(data,
                rowname_col = NULL,
                groupname_col = NULL,
                ...,
                title = NULL,
                width = NULL,
                digit = 1,
                font_family = "arial",
                font_size = 10,
                color = TRUE,
                palette = c("indianred2", "skyblue1"),
                arrange = FALSE) {

  check_table <-
  seq(data) |>
    map(~ data[.] |>
          as_factor() |>
          pull() |>
          levels())

  if (length(check_table) == 2 & identical(check_table[1], check_table[2])) {

    data <-
    data |>
      table() |>
      as_tibble() |>
      pivot_wider(names_from = 2,
                  values_from = n)

    rowname_col <- names(data)[1]
    digit <- 0

  }

  if (!is.null(title)) title <- gt::md(title)
  if (!is.null(width)) width <- gt::px(width)

  if (!color) palette <- c("white", "#444")

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
    gt::tab_header(title = title) |>
    gt::opt_align_table_header(align = "left") |>
    gt::opt_table_font(font = font_family) |>
    gt::tab_options(table.width = width,
                    table.font.size = gt::px(font_size),
                    heading.title.font.size = gt::pct(95),
                    ...) |>
    gt::tab_style(style = gt::cell_borders(style = NULL),
                  locations = list(gt::cells_title(),
                                   gt::cells_column_labels(),
                                   gt::cells_stubhead(),
                                   gt::cells_stub(),
                                   gt::cells_body(),
                                   gt::cells_footnotes())) |>
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
