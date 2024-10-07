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
#' @param row_padding 
#' @param title_align 
#' @param font_size 
#' @param title_font_size 
#' @param stat_font_size 
#' @param pvalue_font_size 
#' @param row_strip 
#' @param footnote_marks 
#' @param footnote_font_size 
#' @param footnote_padding 
#' @param docx 
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
              row_padding = 6,
              title_align = "left",
              font_size = 13,
              title_font_size = font_size + 1, 
              stat_font_size = font_size - 1,
              pvalue_font_size = font_size - 2,
              row_strip = TRUE,
              footnote_marks = "extended",
              footnote_font_size = font_size - 2,
              footnote_padding = row_padding,
              docx = FALSE,
              ...) {
  
  .f <- \(str) str_subset(names(x$`_data`), str)
  
  if (!row_strip) color <- "#ffffff00"
  
  x <-
  tab_options(data = x,
              table.width = width,
              table.font.names = alpha,
              table.font.size = px(font_size),
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
              data_row.padding = px(row_padding),
              data_row.padding.horizontal = px(5),
              row.striping.include_table_body = TRUE,
              row.striping.background_color = bg,
              footnotes.marks = footnote_marks,
              footnotes.font.size = px(footnote_font_size),
              footnotes.padding = px(footnote_padding),
              footnotes.background.color = bg,
              ...)
  
  if (!docx) {
    
    x <-
    x |>
      tab_style(style = cell_text(align = "justify"),
                locations = list(cells_title(), cells_footnotes())) |>
      tab_style(style = cell_text(font = digit),
                locations = cells_body(columns = .f("stat|estimate|p.value"))) |>
      tab_style(style = cell_text(size = px(stat_font_size)),
                locations = cells_body(columns = .f("stat|estimate"))) |>     
      tab_style(style = cell_text(size = px(pvalue_font_size)),
                locations = cells_body(columns = .f("p.value")))
      
  }

  return(x)
  
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

  theme(plot.caption = 
          element_textbox(size = size,
                          hjust = 1,
                          lineheight = 1.05,
                          width = unit(1, "npc"),
                          margin = margin(10, 0, 0, 0)),
        axis.title =
          element_text(size = 9,
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
#' @param title_size 
#' @param title_margin 
#' @param plot_margin 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_risktable <- \(family = "arial",
                     title_size = 7,
                     title_margin = margin(-3, 0, 3, 0),
                     plot_margin = margin(6, 0, -6, 0),
                     ...) {

  list(theme_risktable_default(),
       theme(text = element_text(family = family),
             plot.title = element_text(size = title_size,
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

  theme(text = element_text(family = font),
        legend.position = "none",
        plot.caption = element_textbox(size = 9,
                                       hjust = 1,
                                       lineheight = 1.05,
                                       width = unit(1, "npc"),
                                       margin = margin(10, 0, 0, 0)),
        panel.background = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        plot.caption.position = "plot",
        ...)

}


#' Title
#'
#' @param family 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_blank <- \(family = "arial",
                 ...) {

  .blank <-
  element_rect(color = "white",
               fill = "white")

  .width <- unit(1, "npc")

  theme_void() %+replace%
    theme(plot.background = .blank,
          panel.background = .blank,
          plot.margin = margin(0, 5, 5, 5),
          plot.title =
            element_textbox(size = 9,
                            width = .width,
                            margin = margin(0, 0, 0, 0)),
          plot.caption =
            element_textbox(size = 9,
                            hjust = 1,
                            lineheight = 1.05,
                            width = .width,
                            margin = margin(10, 0, 0, 0)),
          text = element_text(family = family),
          legend.position = "none",
          ...)

}


#' Title
#'
#' @param family 
#' @param title_size 
#' @param title_margin 
#' @param caption_size 
#' @param caption_margin 
#' @param label_size 
#' @param label_margin 
#' @param grid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_infreq <- \(family = "arial",
                  title_size = 13,
                  title_margin = 10,
                  caption_size = 9,
                  caption_margin = 10,
                  label_size = 11,
                  label_margin = -15,
                  grid = TRUE,
                  ...) {

  if (grid) {

    grid <-
    list(panel.grid.major.x =
           element_line(color = "grey95",
                        size = 0.3),
         axis.text.x = 
           element_text(color = "grey90",
                        size = 7,
                        margin = margin(0)))

  } else grid <- NULL

    theme(plot.title.position = "plot",
          plot.title = 
            element_textbox(size = title_size,
                            hjust = 0.5,
                            halign = 0.5,
                            margin = margin(0, 0, title_margin, 0)),
          plot.caption = 
            element_textbox(size = caption_size,
                            hjust = 1,
                            lineheight = 1.05,
                            width = unit(1, "npc"),
                            margin = margin(caption_margin, 0, 0, 0)),
          axis.title = element_blank(),
          axis.text.y = 
            element_text(size = label_size,
                         margin = margin(0, label_margin, 0, 0)),
          text = element_text(family = family),
          plot.caption.position = "plot",
          legend.position = "none",
          panel.background = element_blank(),
          axis.line = element_blank(),
          strip.text = element_blank(),
          axis.ticks = element_blank(),
          axis.text.x = element_blank(),
          ...) %+replace%
      theme(!!!grid)

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
theme_bubble <- \(family = "arial",
                  grid_color = "grey95",
                  size = 13,
                  ...) {

  theme(panel.background = element_blank(),
        panel.grid = element_line(color = grid_color),
        text = 
          element_text(size = size,
                       family = family),
        axis.title = element_text(face = "bold"),
        axis.ticks = element_blank(),
        legend.position = "none",
        ...)
  
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
