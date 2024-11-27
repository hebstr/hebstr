#' Title
#'
#' @param ...
#' @param .auto 
#' @param .abort 
#'
#' @return
#' @export
#'
#' @examples
#'
check_fonts <- \(...,
                 .auto = NULL,
                 .abort = FALSE) {

  if (!is.null(.auto)) {
    
    check_dots_empty()
    
    if (!check_fonts(.auto)) "trebuchet ms" else .auto
    
  } else {
    
    fonts <- unlist(...)
  
    system <- unique(systemfonts::system_fonts()$family)
    system <- glue("\\b{system}\\b")
    system <- glue("(?i){str_u(system)}")
    
    is_installed <- str_detect(fonts, system)
  
    if (FALSE %in% is_installed) {
  
      which_font <-
      data.frame(fonts, is_installed) |>
        filter(!is_installed) |>
        pull(fonts)
      
      if (.abort) cli::cli_abort("{which_font} font{?s} {?is/are} not installed")
      
      return(FALSE)
  
    }
  
    return(TRUE)
    
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
              alpha = check_fonts(.auto = "luciole"),
              digit = check_fonts(.auto = "luciole"),
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
              docx = if (exists("docx")) docx else FALSE,
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
#' @param text_color 
#' @param title_size 
#' @param title_halign 
#' @param title_margin 
#' @param caption_size 
#' @param caption_halign 
#' @param caption_margin 
#' @param grid
#' @param legend_position 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_bar <- \(family = check_fonts(.auto = "luciole"),
               text_color = "#333333",
               title_size = 9,
               title_halign = 1,
               title_margin = margin(0, 0, 0, 0),
               caption_size = 9,
               caption_halign = 0,
               caption_margin = margin(10, 0, 0, 0),
               grid = TRUE,
               legend_position = "none",
               ...) {

  if (!grid) {

    bg <-
    list(panel.background = element_blank(),
         axis.line = element_line(),
         strip.text = element_blank())

  } else bg <- NULL

  theme(text = 
          element_text(family = family,
                       color = text_color),
        plot.title = 
          element_textbox(size = title_size,
                          halign = title_halign,
                          margin = title_margin,
                          lineheight = 1.05,
                          width = unit(1, "npc")),
        plot.caption.position = "plot",
        plot.caption = 
          element_textbox(size = caption_size,
                          halign = caption_halign,
                          margin = caption_margin,
                          lineheight = 1.05,
                          width = unit(1, "npc")),
        axis.title =
          element_text(size = 9,
                       face = "bold"),
        axis.title.x = element_text(vjust = 0.5),
        legend.position = legend_position,
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
theme_tte <- \(family = check_fonts(.auto = "luciole"),
               size = 8,
               vjust_y = 1,
               title_margin = NULL,
               ...) {

  theme_classic() %+replace%
    theme(line = element_line(linewidth = 0.3),
          text = element_text(family = family),
          axis.title = element_text(face = "bold", size = 8),
          axis.title.x = element_text(vjust = -1),
          axis.title.y.left = element_text(vjust = vjust_y),
          axis.text = element_text(size = 7),
          panel.background = element_blank(),
          plot.background = element_blank(),
          plot.margin = margin(0, 0, 0, 0),
          plot.caption = 
            element_textbox(size = size,
                            width = unit(1, "npc"),
                            margin = title_margin),
          plot.caption.position = "plot",
          legend.position = "none",
          ...)

}


#' Title
#'
#' @param family 
#' @param label_size 
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
theme_risktable <- \(family = check_fonts(.auto = "luciole"),
                     label_size = 7,
                     title_size = 7,
                     title_margin = 3,
                     plot_margin = 10,
                     ...) {

  .title_margin <- margin(-title_margin, 0, title_margin, 0)
  .plot_margin <- margin(plot_margin, 0, -plot_margin, 0)
  
  list(theme_risktable_default(),
       theme(text = element_text(family = family),
             plot.title = 
               element_text(size = title_size,
                            face = "bold",
                            margin = .title_margin),
             panel.background = element_blank(),
             plot.background = element_blank(),
             axis.text.y = element_markdown(size = label_size),
             plot.margin = .plot_margin,
             plot.title.position = "plot",
             ...))

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
theme_pca <- \(family = check_fonts(.auto = "luciole"),
               ...) {

  theme(text = element_text(family = family),
        legend.position = "none",
        plot.caption = 
          element_textbox(size = 9,
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
#' @param grid 
#' @param grid_color 
#' @param axis_text_size_y 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
theme_blank <- \(family = check_fonts(.auto = "luciole"),
                 grid = FALSE,
                 grid_color = "grey90",
                 grid_size = 0.2,
                 axis_text_size_y = 7,
                 legend_position = "none",
                 ...) {

  .blank <- element_rect(color = "white", fill = "white")

  .width <- unit(1, "npc")
  
  grid <- if (grid) {
    
    list(panel.grid.major.y =
           element_line(color = grid_color,
                        size = grid_size),
         axis.text.y = 
           element_text(color = grid_color,
                        size = axis_text_size_y))
    
  } else NULL

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
          legend.position = legend_position,
          ...) %+replace%
      theme(!!!grid)

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
theme_infreq <- \(family = check_fonts(.auto = "luciole"),
                  title_size = 11,
                  title_margin = 10,
                  caption_size = 9,
                  caption_margin = 10,
                  label_size = 11,
                  label_margin = margin(r = -15),
                  grid = TRUE,
                  grid_size = 7,
                  ...) {

  if (grid) {

    grid <-
    list(panel.grid.major.x =
           element_line(color = "grey95",
                        size = 0.3),
         axis.text.x = 
           element_text(color = "grey90",
                        size = grid_size,
                        margin = margin(0)))

  } else grid <- NULL

    theme(plot.title.position = "plot",
          plot.title = 
            element_markdown(size = title_size,
                             color = "#333333",
                             hjust = 0.5,
                             halign = 0.5,
                             margin = margin(0, 0, title_margin, 0)),
          plot.caption = 
            element_markdown(size = caption_size,
                             lineheight = 1.05,
                             hjust = 1,
                             halign = 1,
                             margin = margin(caption_margin, 0, 0, 0)),
          axis.title = element_blank(),
          axis.text.y = 
            element_text(size = label_size,
                         margin = label_margin),
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
#' @param family 
#' @param size 
#' @param base_color 
#' @param axis_margin_x 
#' @param axis_margin_y 
#' @param axis_color_x 
#' @param axis_color_y 
#' @param title_color_x 
#' @param title_color_y 
#' @param grid_color_x 
#' @param grid_color_y 
#' @param grid_lighten_x 
#' @param grid_lighten_y 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
theme_bubble <- \(family = check_fonts(.auto = "luciole"),
                  size = 13,
                  base_color = "#333333",
                  axis_margin_x = 12,
                  axis_margin_y = 10,
                  axis_color_x = base_color,
                  axis_color_y = base_color,
                  title_color_x = if (length(axis_color_x) == 1) axis_color_x else base_color,
                  title_color_y = if (length(axis_color_y) == 1) axis_color_y else base_color,
                  grid_color_x = if (all(axis_color_x != base_color)) axis_color_x else "grey95",
                  grid_color_y = if (all(axis_color_y != base_color)) axis_color_y else "grey95",
                  grid_lighten_x = if (all(grid_color_x != "grey95")) 0.85 else 0,
                  grid_lighten_y = if (all(grid_color_y != "grey95")) 0.85 else 0,
                  ...) {

  theme(panel.background = element_blank(),
        panel.grid.major.x = 
          element_line(color = lighten(grid_color_x, grid_lighten_x)),
        panel.grid.major.y = 
          element_line(color = lighten(grid_color_y, grid_lighten_y)),
        text = 
          element_text(size = size, 
                       family = family),
        axis.title =
          element_markdown(face = "bold"),
        axis.title.x = 
          element_markdown(margin = margin(t = axis_margin_x),
                           color = title_color_x),
        axis.title.y =
          element_markdown(margin = margin(l = 20, r = axis_margin_y),
                           color = title_color_y),
        axis.text.x = 
          element_markdown(color = axis_color_x),
        axis.text.y = 
          element_markdown(color = axis_color_y),
        axis.ticks = element_blank(),
        legend.position = "none",
        ...)

}


#' Title
#'
#' @param data 
#' @param head 
#' @param align 
#' @param font_size 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
gt_qmd <- \(data,
            head = FALSE,
            align = "left",
            font_size = 15,
            ...) {
  
  data <- if (!head) gt(data) else gt_preview(data)
  
  data |>
    tab_options(table.font.size = px(font_size),
                column_labels.border.top.color = "white",
                ...) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels()) |> 
    tab_style(style = cell_text(align = align),
              locations = 
                list(cells_column_labels(),
                     cells_body()))
  
}
