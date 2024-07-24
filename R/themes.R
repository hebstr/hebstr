#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
check_font <- \(...) {

  font <- unlist(...)

  sys <- glue("(?i){str_u(system_fonts()$family)}")

  is_installed <- str_detect(font, sys)

  if (FALSE %in% is_installed) {

    which_font <-
    data.frame(font, is_installed) |>
      filter(!is_installed) |>
      pull(font) |>
      unique()

    cli_abort("{which_font} font{?s} {?is/are} not installed")

  }

}


#' Title
#'
#' @param x
#' @param width 
#' @param alpha
#' @param digit
#' @param base
#' @param color
#' @param bg 
#' @param title_align 
#' @param title_font_size 
#' @param table_font_size 
#' @param stat_font_size 
#' @param pvalue_font_size 
#' @param single_line 
#' @param footnote_marks 
#' @param footnote_font_size 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#'
theme_gt <- \(x,
              width = NULL,
              alpha = "arial",
              digit = "arial",
              base = "#333333",
              color = "lightgrey",
              bg = "white",
              title_align = "left",
              title_font_size = 11, 
              table_font_size = 10,
              stat_font_size = 9,
              pvalue_font_size = 8,
              single_line = FALSE,
              footnote_marks = "extended",
              footnote_font_size = 9,
              ...) {
  
  .f <- \(str) str_subset(names(x$`_data`), str)
  
  if (single_line) color <- "#ffffff00"
  
  x <-
  tab_options(data = x,
              table.width = width,
              table.font.names = alpha,
              table.font.size = px(table_font_size),
              table.font.color = base,
              table.background.color = color,
              heading.align = title_align,
              heading.background.color = bg,
              heading.title.font.size = px(title_font_size),
              heading.border.bottom.style = "none",
              heading.padding = px(10),
              column_labels.border.top.style = "none",
              column_labels.border.bottom.width = px(1),
              column_labels.border.bottom.color = base,
              column_labels.background.color = bg,
              table.border.top.style = "none",
              table.border.bottom.style = "none",
              table_body.border.top.width = px(1),
              table_body.border.top.color = base,
              table_body.border.bottom.width = px(1),
              table_body.border.bottom.color = base,
              table_body.hlines.style = "none",
              container.height = pct(100),
              container.width = pct(100),
              data_row.padding = px(4),
              data_row.padding.horizontal = px(5),
              row.striping.include_table_body = TRUE,
              row.striping.background_color = bg,
              footnotes.marks = footnote_marks,
              footnotes.font.size = px(footnote_font_size),
              footnotes.padding = px(5),
              footnotes.background.color = bg,
              ...)
  
  x |>
    tab_style(style = cell_text(align = "justify"),
              locations = list(cells_title(),
                               cells_footnotes())) |>
    tab_style(style = cell_text(font = digit),
              locations = cells_body(columns = .f("stat|estimate|p.value"))) |>
    tab_style(style = cell_text(size = px(stat_font_size)),
              locations = cells_body(columns = .f("stat|estimate"))) |>     
    tab_style(style = cell_text(size = px(pvalue_font_size)),
              locations = cells_body(columns = .f("p.value")))

}


#' Title
#'
#' @param family
#' @param size 
#' @param grid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_bar <- \(family = "arial",
               size = 9,
               grid = TRUE,
               ...) {

  if (!grid) {

    bg <-
    list(panel.background = element_blank(),
         axis.line = element_line(),
         strip.text = element_blank())

  } else bg <- NULL

  theme(plot.caption = element_textbox(size = size,
                                       hjust = 1,
                                       lineheight = 1.05,
                                       width = unit(1, "npc"),
                                       margin = margin(10, 0, 0, 0)),
        axis.title = element_text(size = 9,
                                  face = "bold"),
        axis.title.x = element_text(vjust = 0.5),
        text = element_text(family = family),
        plot.caption.position = "plot",
        legend.position = "none",
        ...) %+replace%
          inject(theme(!!!bg))

}


#' Title
#'
#' @param family 
#' @param size
#' @param vjust_y 
#' @param title_margin 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
theme_tte <- \(family = "arial",
               size = 8,
               vjust_y = 1,
               title_margin = NULL,
               ...) {

  theme_classic() %+replace%
    theme(line = element_line(linewidth = 0.3),
          text = element_text(family = family),
          axis.title = element_text(face = "bold",
                                    size = 8),
          axis.title.x = element_text(vjust = -1),
          axis.title.y.left = element_text(vjust = vjust_y),
          axis.text = element_text(size = 7),
          panel.background = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0, 0, 0, 0),
          plot.caption = element_textbox(size = size,
                                         width = unit(1, "npc"),
                                         margin = title_margin),
          plot.caption.position = "plot",
          legend.position = "none",
          ...)

}


#' Title
#'
#' @param family 
#' @param plot_margin 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_risktable <- \(family = "arial",
                     title_margin = margin(-3, 0, 3, 0),
                     plot_margin = margin(6, 0, -6, 0),
                     ...) {

  list(theme_risktable_default(),
       theme(text = element_text(family = family),
             plot.title = element_text(size = 6,
                                       face = "bold",
                                       margin = title_margin),
             panel.background = element_blank(),
             plot.background = element_blank(),
             axis.text.y = element_markdown(size = 7),
             plot.margin = plot_margin,
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
#' @param family 
#' @param size 
#'
#' @return
#' @export
#'
#' @examples
#'
theme_wrap <- \(family = "arial",
                size = 11) {

  theme(text = element_text(family = family),
        plot.caption = element_textbox(size = size,
                                       hjust = 1,
                                       lineheight = 1.05,
                                       width = unit(1, "npc"),
                                       margin = margin(10, 0, 0, 0)))

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
theme_infreq <- \(font = "arial",
                  bg = TRUE,
                  ...) {

  if (!bg) {

    bg <-
    list(panel.background = ggplot2::element_blank(),
         axis.line = ggplot2::element_line(),
         strip.text = ggplot2::element_blank())

  } else bg <- NULL

    ggplot2::theme(plot.title = ggtext::element_textbox(size = 9,
                                                        hjust = 0.5,
                                                        halign = 0.5,
                                                        margin = ggplot2::margin(0, 0, 8, 0)),
                   plot.title.position = "plot",
                   plot.caption = ggtext::element_textbox(size = 9,
                                                          hjust = 1,
                                                          lineheight = 1.05,
                                                          width = ggplot2::unit(1, "npc"),
                                                          margin = ggplot2::margin(10, 0, 0, 0)),
                   axis.title = ggplot2::element_blank(),
                   axis.text.y = element_text(size = 7,
                                              margin = ggplot2::margin(0, -10, 0, 0)),
                   text = element_text(family = font),
                   plot.caption.position = "plot",
                   legend.position = "none",
                   panel.background = ggplot2::element_blank(),
                   axis.line = ggplot2::element_blank(),
                   strip.text = ggplot2::element_blank(),
                   axis.ticks = ggplot2::element_blank(),
                   axis.text.x = element_blank(),
                   ...)

}


#' Title
#'
#' @param y
#' @param color
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
add_label <- \(y,
               color = NULL,
               ...) {

  if (is.null(color)) {

    list(geom_text(aes(y = ggplot2::after_stat(count) + y * max(count),
                       label = ggplot2::after_stat(count),
                       color = ggplot2::after_scale(fill)),
                   ...),
         geom_text(aes(y = ggplot2::after_stat(count) - y * max(count),
                       label = scales::percent(ggplot2::after_stat(count) / sum(ggplot2::after_stat(count)),
                                               accuracy = 0.1),
                       color = ggplot2::after_scale(fill |> colorspace::lighten(0.95))),
                   ...))

  } else {

    list(geom_text(aes(y = ggplot2::after_stat(count) + y * max(count),
                       label = ggplot2::after_stat(count)),
                   color = color,
                   ...),
         geom_text(aes(y = ggplot2::after_stat(count) - y * max(count),
                       label = scales::percent(ggplot2::after_stat(count) / sum(ggplot2::after_stat(count)),
                                               accuracy = 0.1)),
                   color = colorspace::lighten(color, 0.95),
                   ...))

  }

}


#' Title
#'
#' @param data 
#' @param head 
#' @param size 
#' @param align 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
gt_qmd <- \(data,
            head = FALSE,
            size = 15,
            align = "center",
            ...) {
  
  data <-
  if ("gtsummary" %in% class(data)) as_gt(data)
  else if (!head) gt(data) else gt_preview(data)
  
  data |>
    tab_options(table.font.size = px(size),
                column_labels.border.top.color = "white",
                ...) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |> 
    tab_style(style = cell_text(align = align),
              locations = 
                list(cells_column_labels(),
                     cells_body()))
  
}
