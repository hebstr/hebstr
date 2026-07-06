#' Bar chart of frequency counts with percentage labels
#'
#' Generates a bar chart displaying the frequency counts and percentages of a
#' categorical variable. Count bars are combined with numeric count labels above
#' the bars and percentage annotations below them, for descriptive statistical
#' reporting.
#'
#' @param data A data frame containing the variable to plot.
#' @param var Name of the categorical variable to analyse (as a character
#'   string). Frequency counts are computed on this variable.
#' @param color Hex color code or CSS name for the bar fill.
#'   Default `"#333333"` (dark grey).
#' @param alpha Bar transparency, between 0 (transparent) and 1 (opaque).
#'   Default `0.7`.
#' @param nudge_y Vertical offset for label positioning, as a proportion of the
#'   maximum bar height. Default `0.04`.
#' @param pct_min Minimum count threshold for displaying percentages, as a
#'   proportion of the total count. Default `0.05`.
#' @param ylab Label for the y axis. Default `"Effectif"`.
#' @param family Font family for the text labels. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param grid Logical controlling display of the background grid.
#'   Default `TRUE`.
#' @param ... Additional arguments forwarded to the text layers (`geom_text`)
#'   for further label customization.
#'
#' @return A ggplot object: the bar chart with counts and percentages, ready to
#'   display or export.
#'
#' @section Plot layers:
#' The plot combines three data layers. The base layer draws the count bars with
#' the given color and transparency. The first text layer places the numeric
#' counts above the bars. The second text layer places the corresponding
#' percentages below the bars, but only for levels above the `pct_min` threshold.
#'
#' @section Required helpers:
#' The function relies on several specialized helpers that must be available.
#' `check_fonts()` handles automatic detection of system fonts. `pct_min()`
#' filters levels by the minimum percentage threshold. `label_p()` formats
#' percentages following French typographic conventions. `theme_bar()` applies
#' the standardized bar-chart theme.
#'
#' @section Automatic labelling:
#' The function uses `var_label()` to extract the descriptive label of the
#' analysed variable. When no label is set, the variable name is used instead.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(dplyr)
#'
#' # Basic use with the mpg dataset (ggplot2)
#' ggcount(data = mpg,
#'         var = "class")
#'
#' # Within an analysis pipeline with filtering
#' mpg |>
#'   filter(year == 2008) |>
#'   ggcount(var = "class",
#'           color = "#0099FF")
#'
#' # Advanced label customization
#' mpg |>
#'   ggcount(var = "fl",
#'           color = "#FF0000",
#'           size = 3.5,
#'           fontface = "bold",
#'           grid = FALSE)
#'
#' # Data with an unbalanced distribution
#' storms |>
#'   filter(year >= 2010) |>
#'   mutate(status = forcats::fct_infreq(status)) |>
#'   ggcount(var = "status",
#'           pct_min = 0.1)
#'}
#'
#' @family visualization functions
#' @seealso
#' * [ggplot2::geom_bar()] for the base bar chart
#' * [ggplot2::geom_text()] for adding text labels
#' * [labelled::var_label()] for variable-label handling
#' * [scales::label_percent()] for percentage formatting
#'
#' @export
ggcount <- \(
  data,
  var,
  color = "#333333",
  alpha = 0.7,
  nudge_y = 0.04,
  pct_min = 0.05,
  ylab = "Effectif",
  family = .text_font(),
  grid = TRUE,
  ...
) {
  if (!var %in% names(data)) {
    cli_abort("{.arg var} ({.val {var}}) is not a column of {.arg data}.")
  }

  data |>
    ggplot(aes(x = .data[[var]], fill = I(color))) +
    geom_bar(alpha = alpha) +
    geom_text(
      mapping = aes(
        y = after_stat(count + nudge_y * max(count)),
        label = after_stat(count),
        color = after_scale(fill)
      ),
      stat = "count",
      family = family,
      ...
    ) +
    geom_text(
      data = ~ pct_min(., var, pct_min),
      mapping = aes(
        y = after_stat(count - nudge_y * max(count)),
        label = label_p()(after_stat(count) / nrow(data)),
        color = after_scale(fill |> lighten(0.95))
      ),
      stat = "count",
      family = family,
      ...
    ) +
    labs(x = var_label(data[[var]]) %||% var, y = ylab) +
    theme_bar(family = family, grid = grid)
}
