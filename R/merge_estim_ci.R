#' Merge estimate and confidence interval into a single formatted column
#'
#' Combines a point estimate column and its confidence interval columns into
#' a single character column (e.g. `"1.23 [0.98; 1.50]"`).
#'
#' @param data A data frame containing the estimate and CI columns.
#' @param estim_col String. Name of the estimate column. Default `"estimate"`.
#' @param ci_col Tidy-select. Columns containing the CI bounds.
#'   Default `starts_with("conf.")`.
#' @param name String. Name of the output column. Supports glue syntax.
#'   Default `"estimate_ci"`.
#' @param ci_data A glue template for formatting the CI
#'   (e.g. `"[{conf.low}; {conf.high}]"`). Default from `check_opts(ci$data)`.
#' @param digit Integer. Number of decimal places. Default `2`.
#' @param percent If `TRUE`, multiply values by 100 before formatting.
#' @param keep If `TRUE`, keep the original estimate and CI columns.
#'
#' @return A data frame with the merged column (and optionally the originals).
#' @export
#'
#' @examples
#' df <- data.frame(estimate = 1.234, conf.low = 0.5, conf.high = 2.0)
#' merge_estim_ci(df, ci_data = "[{conf.low}; {conf.high}]")
#'
merge_estim_ci <- \(
  data,
  estim_col = "estimate",
  ci_col = starts_with("conf."),
  name = "estimate_ci",
  ci_data = check_opts(ci$data),
  digit = 2,
  percent = FALSE,
  keep = FALSE
) {
  name <- str_glue(name)

  vars <- expr(c(all_of(estim_col), all_of(ci_col)))

  multi <- if (percent) 100 else 1

  data <- mutate(
    .data = data,
    across(!!vars, ~ round(. * multi, digit) |> format(nsmall = digit)),
    "{name}" := str_c(.data[[estim_col]], " ", str_glue(ci_data))
  )

  if (!keep) {
    data <- select(data, -!!vars)
  }

  data
}
