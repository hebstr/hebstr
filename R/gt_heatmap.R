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
                font_family = check_fonts(.auto = "luciole"),
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

  if (length(check_table) == 2 && identical(check_table[1], check_table[2])) {

    data <-
    data |>
      table() |>
      as_tibble() |>
      pivot_wider(names_from = 2,
                  values_from = n)

    rowname_col <- names(data)[1]
    digit <- 0

  }

  if (!is.null(title)) title <- md(title)
  if (!is.null(width)) width <- px(width)

  if (!color) palette <- c("white", "#444")

  if (arrange) {

    data <-
    data |>
      rowwise() |>
        mutate(.sum = sum(across(where(is.numeric)))) |>
        ungroup() |>
      arrange(desc(.sum)) |>
      select(-.sum)

  }

  limit <-
  data |>
    pivot_longer(cols = where(is.numeric)) |>
    pull(value) |>
    max()

  data |>
    gt(rowname_col = rowname_col,
       groupname_col = groupname_col) |>
    tab_header(title = title) |>
    opt_align_table_header(align = "left") |>
    opt_table_font(font = font_family) |>
    tab_options(table.width = width,
                table.font.size = px(font_size),
                heading.title.font.size = pct(95),
                ...) |>
    tab_style(style = cell_borders(style = NULL),
              locations =
                list(cells_title(),
                     cells_column_labels(),
                     cells_stubhead(),
                     cells_stub(),
                     cells_body(),
                     cells_footnotes())) |>
    tab_style(style = 
                cell_text(align = "center",
                          weight = "bold"),
              locations = cells_column_labels()) |>
    tab_style(style = 
                cell_text(align = "right",
                          weight = "bold"),
              locations = cells_stub()) |>
    tab_style(style = cell_text(align = "center"),
              locations = cells_body()) |>
    fmt_number(columns = where(is.numeric),
               decimals = digit) %>%
    data_color(method = "numeric",
               palette = c(palette[1], "white", palette[2]),
               domain = c(-limit, limit))

}
