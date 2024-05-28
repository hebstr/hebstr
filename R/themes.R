#' Title
#'
#' @param x
#' @param width
#' @param alpha
#' @param digit
#' @param base
#' @param color
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_gt <- \(x,
              alpha = "arial",
              digit = "arial",
              base = "#333333",
              color = "lightgrey",
              bg = "white",
              ...) {

x |>
  gt::opt_align_table_header(align = "left") |>
  gt::opt_table_font(font = alpha) |>
  gt::tab_options(table.font.size = gt::px(12),
                  table.font.color = base,
                  table.background.color = color,
                  heading.background.color = bg,
                  heading.title.font.size = gt::pct(95),
                  heading.border.bottom.style = "none",
                  table.border.top.style = "none",
                  table.border.bottom.style = "none",
                  column_labels.border.top.style = "none",
                  column_labels.border.bottom.width = gt::px(1),
                  column_labels.border.bottom.color = base,
                  column_labels.background.color = bg,
                  table_body.border.top.width = gt::px(1),
                  table_body.border.top.color = base,
                  table_body.border.bottom.width = gt::px(1),
                  table_body.border.bottom.color = base,
                  table.border.bottom.width = gt::px(1),
                  table.border.bottom.color = base,
                  table_body.hlines.style = "none",
                  container.padding.x = gt::px(10),
                  heading.padding = gt::px(10),
                  data_row.padding = gt::px(3),
                  data_row.padding.horizontal = gt::px(5),
                  row.striping.include_table_body = TRUE,
                  row.striping.background_color = bg,
                  footnotes.marks = "standard",
                  footnotes.background.color = bg,
                  footnotes.padding = gt::px(1),
                  footnotes.font.size = gt::pct(80),
                  ...) |>
  gt::tab_style(style = gt::cell_text(align = "justify"),
                locations = list(gt::cells_title(),
                                 gt::cells_footnotes())) |>
  gt::tab_style(style = gt::cell_text(size = gt::px(11)),
                locations = gt::cells_body(columns = grep("p.v", names(x[[1]])))) |>
  gt::tab_style(style = gt::cell_text(font = digit),
                locations = gt::cells_body(columns =
                                             purrr::map(c("stat", "p.v", "estim"),
                                                        ~ grep(., names(x[[1]]))) |>
                                             unlist()))

}


#' Title
#'
#' @param font
#' @param bg
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_bar <- \(font = "arial",
               bg = TRUE,
               ...) {

  if (!bg) {

    bg <-
    list(panel.background = ggplot2::element_blank(),
         axis.line = ggplot2::element_line(),
         strip.text = ggplot2::element_blank())

  } else bg <- NULL

  ggplot2::theme(plot.caption = ggtext::element_textbox(size = 9,
                                                        hjust = 1,
                                                        lineheight = 1.05,
                                                        width = ggplot2::unit(1, "npc"),
                                                        margin = ggplot2::margin(10, 0, 0, 0)),
                 axis.title = ggplot2::element_text(size = 9,
                                                    face = "bold"),
                 axis.title.x = ggplot2::element_text(vjust = 0.5),
                 text = ggplot2::element_text(family = font),
                 plot.caption.position = "plot",
                 legend.position = "none",
                 ...) %+replace%
                   rlang::inject(ggplot2::theme(!!!bg))

}


#' Title
#'
#' @param font
#' @param size
#' @param margin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_tte <- \(font = "arial",
               size = 9,
               margin = ggplot2::margin(0, 0, 0, 0),
               ...) {

  ggplot2::theme_classic() %+replace%
    ggplot2::theme(line = ggplot2::element_line(linewidth = 0.3),
                  text = ggplot2::element_text(family = font),
                  axis.title = ggplot2::element_text(face = "bold",
                                                     size = 8),
                  axis.title.x = ggplot2::element_text(vjust = -1),
                  axis.title.y.left = ggplot2::element_text(vjust = 1),
                  axis.text = ggplot2::element_text(size = 8),
                  panel.background = ggplot2::element_blank(),
                  plot.background = ggplot2::element_blank(),
                  plot.margin = ggplot2::margin(0, 0, 0, 0),
                  plot.caption = ggtext::element_textbox(size = size,
                                                         width = ggplot2::unit(1, "npc"),
                                                         margin = margin),
                  plot.caption.position = "plot",
                  legend.position = "none",
                  ...)

}


#' Title
#'
#' @param font
#' @param table_margin
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_risktable <- \(font = "arial",
                     table_margin = ggplot2::margin(0, 0, 0, 0),
                     ...) {

  list(ggsurvfit::theme_risktable_default(),
       ggplot2::theme(text = ggplot2::element_text(family = font),
             plot.title = ggplot2::element_text(size = 7.5,
                                                face = "bold",
                                                margin = ggplot2::margin(0, 0, 0, 0)),
             panel.background = ggplot2::element_blank(),
             plot.background = ggplot2::element_blank(),
             axis.text.y = ggplot2::element_text(size = 7),
             plot.margin = table_margin,
             plot.title.position = "plot",
             ...))

}


#' Title
#'
#' @param font
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_pca <- \(font = "arial",
               ...) {

  ggplot2::theme(text = ggplot2::element_text(family = font),
                 legend.position = "none",
                 plot.caption = ggtext::element_textbox(size = 9,
                                                        hjust = 1,
                                                        lineheight = 1.05,
                                                        width = ggplot2::unit(1, "npc"),
                                                        margin = ggplot2::margin(10, 0, 0, 0)),
                 panel.background = ggplot2::element_blank(),
                 axis.text = ggplot2::element_blank(),
                 axis.title = ggplot2::element_blank(),
                 axis.ticks = ggplot2::element_blank(),
                 plot.caption.position = "plot",
                 ...)

}


#' Title
#'
#' @param font
#'
#' @return
#' @export
#'
#' @examples
#'
theme_wrap <- \(font = "arial") {

  ggplot2::theme(text = element_text(family = font),
                 plot.caption = ggtext::element_markdown(size = 11, hjust = 0))

}


#' Title
#'
#' @param font
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_blank <- \(font = "arial",
                 ...) {

  .blank <-
  ggplot2::element_rect(color = "white",
                        fill = "white")

  .width <- ggplot2::unit(1, "npc")

  ggplot2::theme_void() %+replace%
    ggplot2::theme(plot.background = .blank,
                   panel.background = .blank,
                   plot.margin = ggplot2::margin(0, 5, 5, 5),
                   plot.title =
                     ggtext::element_textbox(size = 9,
                                             width = .width,
                                             margin = ggplot2::margin(0, 0, 0, 0)),
                   plot.caption =
                     ggtext::element_textbox(size = 9,
                                             hjust = 1,
                                             lineheight = 1.05,
                                             width = .width,
                                             margin = ggplot2::margin(10, 0, 0, 0)),
                   text = ggplot2::element_text(family = font),
                   legend.position = "none",
                   ...)

}
