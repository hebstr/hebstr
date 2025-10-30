#' Title
#'
#' @param ... arg
#' @param .default arg 
#' @param .auto arg 
#' @param .abort arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
check_fonts <- \(...,
                 .default = "trebuchet ms",
                 .auto = NULL,
                 .abort = FALSE) {

  if (!is.null(.auto)) {
    
    check_dots_empty()
    
    if (!check_fonts(.auto)) .default else .auto
    
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
#' @param x arg
#' @param width arg 
#' @param alpha arg
#' @param digit arg
#' @param base arg
#' @param color arg
#' @param bg arg 
#' @param row_padding arg 
#' @param title_align arg 
#' @param font_size arg 
#' @param title_font_size arg 
#' @param stat_font_size arg 
#' @param pvalue_font_size arg 
#' @param row_strip arg 
#' @param footnote_marks arg 
#' @param footnote_font_size arg 
#' @param footnote_padding arg 
#' @param docx arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
theme_gt <- \(x,
              width = NULL,
              alpha = check_opts(font),
              digit = check_opts(font),
              base = "#333333",
              color = check_opts(color$cold[1]),
              bg = "white",
              row_padding = 8,
              title_align = "left",
              font_size = 13,
              title_font_size = font_size + 1, 
              stat_font_size = font_size - 1,
              pvalue_font_size = font_size - 2,
              row_strip = TRUE,
              footnote_marks = "extended",
              footnote_font_size = font_size - 2,
              footnote_padding = row_padding,
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
  
  if (!exists("docx", envir = globalenv())) {
    
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
#' @param family arg
#' @param text_color arg 
#' @param title_size arg 
#' @param title_halign arg 
#' @param title_margin arg 
#' @param caption_size arg 
#' @param caption_halign arg 
#' @param caption_margin arg 
#' @param grid arg
#' @param legend_position arg 
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
        legend.position = legend_position) %+replace%
          inject(theme(!!!bg, ...))

}


#' Title
#'
#' @param family arg
#' @param size arg
#' @param vjust_y arg 
#' @param title_margin arg 
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
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
#' @param family arg
#' @param label_size arg 
#' @param title_size arg 
#' @param title_margin arg 
#' @param plot_margin arg 
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
#' @param family arg 
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
        plot.caption.position = "plot") %+replace%
    theme(...)

}


#' Title
#'
#' @param family arg 
#' @param grid arg 
#' @param grid_color arg 
#' @param axis_text_size_y arg 
#' @param grid_size arg 
#' @param legend_position arg 
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
#' @param family arg
#' @param title_size arg 
#' @param title_margin arg 
#' @param caption_size arg 
#' @param caption_margin arg 
#' @param label_size arg 
#' @param label_margin arg 
#' @param grid arg
#' @param grid_size arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
#' @param family arg 
#' @param size arg 
#' @param base_color arg 
#' @param axis_margin_x arg 
#' @param axis_margin_y arg 
#' @param axis_color_x arg 
#' @param axis_color_y arg 
#' @param title_color_x arg 
#' @param title_color_y arg 
#' @param grid_color_x arg 
#' @param grid_color_y arg 
#' @param grid_lighten_x arg 
#' @param grid_lighten_y arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
#' @param data arg
#' @param head arg 
#' @param font_family arg 
#' @param font_size arg 
#' @param id arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
gt_qmd <- \(data,
            head = NULL,
            font_family = "luciole",
            font_size = 15,
            id = "tbl-id",
            ...) {
  
  if ("gtsummary" %in% class(data)) {
    
    data <- as_gt(data)
  
  } else {
    
    if (is.null(head)) {
      
      data <- gt(data, id = id) 
      
    } else {
      
      data <- gt_preview(data, top_n = head)
      
    }
    
  }
  
  data |>
    tab_options(table.font.names = c(font_family, "system-ui"),
                table.font.size = px(font_size),
                column_labels.border.top.color = "white",
                ...) |>
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_column_labels())
  
}

#' Title
#'
#' @param x arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#' 
glue_qmd <- \(x) {
  
  glue(x, .open = "<<", .close = ">>", .envir = parent.frame())
  
}

#' Title
#'
#' @param src arg
#' @param lang arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#' 
include_code_file <- \(src, lang = "r") {
  
  glue_qmd("
  ```{.<<lang>> include='<<src>>' code-line-numbers='true'}
  ```
  ")
  
}

#' Title
#'
#' @param src arg
#' @param name arg
#' @param lang arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#' 
add_code_file <- \(src, name = NULL, lang = "r") {
  
  if (is.null(name)) name <- str_remove(src, ".+/")
  
  glue_qmd("
  ::: {add-from=<<src>> code-line-numbers='true' code-filename=<<name>>}
  ```<<lang>>
  ```
  :::
  ")
  
}
