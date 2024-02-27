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

  ggplot2::theme(legend.position = "none",
                 plot.caption = ggtext::element_textbox(size = 9,
                                                        hjust = 1,
                                                        lineheight = 1.05,
                                                        width = ggplot2::unit(1, "npc"),
                                                        margin = ggplot2::margin(10, 0, 0, 0)),
                 plot.caption.position = "plot",
                 axis.title = ggplot2::element_text(size = 9,
                                                    face = "bold"),
                 axis.title.x = ggplot2::element_text(vjust = 0.5),
                 text = ggplot2::element_text(family = font),
                 ...) %+replace%
                   rlang::inject(ggplot2::theme(!!!bg))

}

