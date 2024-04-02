#' Title
#'
#' @param data
#' @param parametric
#' @param qt_stat
#' @param ql_stat
#'
#' @return
#' @export
#'
#' @examples
#'
easy_descr <- \(data,
                parametric = NULL) {

  cli::cli_h1("easy_descr")
  cli::cli_text("\n\n")

### QT DATA -------------------------------------------------------------------------------

  qt_total <- dplyr::expr(dplyr::where(~ is.numeric(.) & length(unique(.)) != 2))

  nonparametric <-
  data |>
    dplyr::select(c(eval(total), -dplyr::all_of(parametric))) |>
    names() |>
    dplyr::expr()

  qt_stat <- list(min = c("Min" = "{min}"),
                  q1 = c("Q1" = "{p25}"),
                  median_iqr = c("Médiane (IQR)" = "{median} ({IQR})"),
                  q3 = c("Q3" = "{p75}"),
                  max = c("Max" = "{max}"),
                  mean_sd = c("Moyenne±SD" = "{mean}±{sd}"))

### QL DATA -------------------------------------------------------------------------------

  ql_vars <-
  data |>
    dplyr::select(where(~ !is.numeric(.))) |>
    names()

  ql_stat <- list(n_pct = c("n (%)" = "{n} ({p})"))

### BIN DATA -------------------------------------------------------------------------------

  bin_vars <-
  data |>
    dplyr::select(-eval(qt_total), -dplyr::all_of(ql_vars)) |>
    names()

### LIST ------------------------------------------------------------------------------

  descr <-
  dplyr::lst(qt = dplyr::lst(vars = dplyr::lst(total = eval(qt_total),
                                               parametric = parametric,
                                               nonparametric = eval(nonparametric)),
                             stat = qt_stat,
                             spanner = names(purrr::list_c(stat))),
             ql = dplyr::lst(vars = ql_vars,
                             stat = ql_stat,
                             spanner = names(purrr::list_c(stat))),
             bin = dplyr::lst(vars = bin_vars,
                              stat = ql_stat,
                              spanner = names(purrr::list_c(stat))))

  cli_qt_total_length <-
  data |>
    dplyr::select(eval(descr$qt$vars$total)) |>
    length()

  cli_qt_p <- stringr::str_flatten_comma(descr$qt$vars$parametric)
  cli_qt_np <- stringr::str_flatten_comma(descr$qt$vars$nonparametric)
  cli_ql <- stringr::str_flatten_comma(descr$ql$vars)
  cli_bin <- stringr::str_flatten_comma(descr$bin$vars)

### CLI -------------------------------------------------------------------------------

  cli::cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong Quantitative:} {cli_qt_total_length}")
    cli::cli_li(c("Parametric: {cli_qt_p}",
                  "Non-parametric: {cli_qt_np}"))
  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong Qualitative:} {length(descr$ql$vars)}")
    cli::cli_li("{cli_ql}")
  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong Binary:} {length(descr$bin$vars)}")
    cli::cli_li("{cli_bin}")
  cli::cli_text("\n\n")
  cli::cli_rule()

  return(descr)

}
