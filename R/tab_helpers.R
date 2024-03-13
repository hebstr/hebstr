#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
all_dicho <- \(data, ...) {

  dots <- c(...)

  level <-
  purrr::map_int(dots,
                 ~ with(data, get(.)) |>
                   nlevels())

  dots[level == 2]

}


#' Title
#'
#' @param data
#' @param estim_col
#' @param ci_col
#' @param merge_col
#' @param ci_data
#' @param estim_ci_digit
#' @param pvalue_digit
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
#'
merge_estim_ci <- \(data,
                    estim_col = "estimate",
                    ci_col = starts_with("conf."),
                    merge_col = "estimate_ci",
                    ci_data,
                    estim_ci_digit = 2,
                    percent = FALSE) {

  if (percent) multi <- 100 else multi <- 1

  .rnd <- \(x, n) {

    x |>
    round(n) |>
      format(nsmall = n)

  }

  data |>
    mutate(across(c(all_of(estim_col), all_of(ci_col)),
                  ~ .rnd(. * multi, estim_ci_digit)),
           "{merge_col}" := glue::glue("{get(estim_col)} ", ci_data))

}


#' Title
#'
#' @param data
#' @param var
#' @param new_lab
#' @param ref
#'
#' @return
#' @export
#'
#' @examples
#'
easy_relab <- \(data,
                var,
                new_lab,
                ref = " — ref: ") {

  var <- rlang::enexpr(var)

  .lab <- "{glue::glue(new_lab)}{ref}{tolower(reference_level)}"

  data |>
    gtsummary::modify_table_body(
      ~ . |>
        dplyr::mutate(label = ifelse(variable == var,
                                     glue::glue(.lab),
                                     label))
    )

}
