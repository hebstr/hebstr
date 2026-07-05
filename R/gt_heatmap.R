#' Colored heatmap table from a cross-tabulation
#'
#' Renders a numeric table as a [gt::gt()] heatmap, coloring cells on a diverging
#' scale centered on zero. A pair of factor columns sharing the same levels is
#' cross-tabulated into a contingency table before display.
#'
#' @param data A data frame of numeric values to display, or two factor columns
#'   sharing the same levels to cross-tabulate into a contingency table.
#' @param rowname_col Column to use for the row (stub) labels, passed to
#'   [gt::gt()].
#' @param groupname_col Column to use for the row groups, passed to [gt::gt()].
#' @param ... Additional table options forwarded to [gt::tab_options()].
#' @param title Table title, rendered as Markdown.
#' @param width Table width, in pixels.
#' @param digit Number of decimal places for the numeric cells.
#' @param font_family Font family for the table's text. When [set_opts()] has
#'   been called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise falls back to Luciole, or the OS-agnostic system sans-serif
#'   (`"sans"`) when Luciole is unavailable.
#' @param font_size Text size, in pixels.
#' @param color Whether to color cells with `palette`. When `FALSE`, cells use a
#'   neutral grey scale.
#' @param palette Length-two vector of colors for the low and high ends of the
#'   diverging scale; white is the midpoint.
#' @param autocolor_text Whether to pick each cell's text color automatically for
#'   contrast against its background.
#' @param arrange Whether to sort rows in descending order of their numeric row
#'   sum.
#'
#' @return A [gt::gt()] table object (`gt_tbl`).
#' @export
#'
#' @examples "arg"
#'
gt_heatmap <- \(
  data,
  rowname_col = NULL,
  groupname_col = NULL,
  ...,
  title = NULL,
  width = NULL,
  digit = 1,
  font_family = .text_font(),
  font_size = 10,
  color = TRUE,
  palette = c("indianred2", "skyblue1"),
  autocolor_text = TRUE,
  arrange = FALSE
) {
  check_table <-
    seq(data) |>
    map(
      ~ data[.] |>
        as_factor() |>
        pull() |>
        levels()
    )

  if (length(check_table) == 2 && identical(check_table[1], check_table[2])) {
    data <-
      data |>
      table() |>
      as_tibble() |>
      pivot_wider(names_from = 2, values_from = n)

    rowname_col <- names(data)[1]
    digit <- 0
  }

  if (!is.null(title)) {
    title <- md(title)
  }
  if (!is.null(width)) {
    width <- px(width)
  }

  if (!color) {
    palette <- c("white", "#444")
  }

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
    abs() |>
    max(na.rm = TRUE)

  data |>
    gt(rowname_col = rowname_col, groupname_col = groupname_col) |>
    tab_header(title = title) |>
    opt_align_table_header(align = "left") |>
    opt_table_font(font = font_family) |>
    tab_options(
      table.width = width,
      table.font.size = px(font_size),
      heading.title.font.size = pct(95),
      ...
    ) |>
    tab_style(
      style = cell_borders(style = NULL),
      locations = list(
        cells_title(),
        cells_column_labels(),
        cells_stubhead(),
        cells_stub(),
        cells_body(),
        cells_footnotes()
      )
    ) |>
    tab_style(
      style = cell_text(align = "center", weight = "bold"),
      locations = cells_column_labels()
    ) |>
    tab_style(
      style = cell_text(align = "right", weight = "bold"),
      locations = cells_stub()
    ) |>
    tab_style(style = cell_text(align = "center"), locations = cells_body()) |>
    fmt_number(
      columns = where(is.numeric),
      dec_mark = getOption("OutDec"),
      sep_mark = ifelse(getOption("OutDec") == ".", ",", " "),
      decimals = digit
    ) |>
    data_color(
      method = "numeric",
      palette = c(palette[1], "white", palette[2]),
      domain = c(-limit, limit),
      autocolor_text = autocolor_text
    )
}
