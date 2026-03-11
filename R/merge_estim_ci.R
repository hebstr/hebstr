#' Title
#'
#' @param data arg
#' @param estim_col arg
#' @param ci_col arg
#' @param name arg
#' @param ci_data arg
#' @param digit arg
#' @param percent arg
#' @param keep arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
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
    "{name}" := str_glue("{get(estim_col)} ", ci_data)
  )

  if (!keep) data <- select(data, -!!vars)

  data

}
