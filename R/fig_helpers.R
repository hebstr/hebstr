#' Title
#'
#' @param data
#' @param var
#' @param color
#' @param alpha
#' @param nudge_y
#' @param pct_min
#' @param ylab
#' @param family
#' @param grid
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
ggcount <- \(data,
             var,
             color = "#333333",
             alpha = 0.7,
             nudge_y = 0.04,
             pct_min = 0.05,
             ylab = "Effectif",
             family = check_fonts(.auto = "luciole"),
             grid = TRUE,
             ...) {

  data |>
    ggplot(aes(x = get(var),
               fill = I(color))) +
    geom_bar(alpha = alpha) +
    geom_text(mapping =
                aes(y = after_stat(count + nudge_y * max(count)),
                    label = after_stat(count),
                    color = after_scale(fill)),
              stat = "count",
              family = family,
              ...) +
    geom_text(data = ~ pct_min(., var, pct_min),
              mapping = 
                aes(y = after_stat(count - nudge_y * max(count)),
                    label = label_p()(after_stat(count) / nrow(data)),
                    color = after_scale(fill |> lighten(0.95))),
              stat = "count",
              family = family,
              ...) +
    labs(x = var_label(data[[var]]) %||% var,
         y = ylab) +
    theme_bar(family = family,
              grid = grid)

}
