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
                parametric = NULL,
                qt_stat = NULL,
                ql_stat = NULL) {

  cli_h1("easy_descr")
  cli_text("\n\n")

### QT DATA -------------------------------------------------------------------------------

  qt_vars <-
  lst(total = where(~ is.numeric(.) & length(unique(na.omit(.))) != 2),
      parametric = parametric,
      nonparametric =
        data |>
          select(c(eval(total), -all_of(parametric))) |>
          names())

  qt_stat <-
  list(min = c("Min" = "{min}"),
       q1 = c("Q1" = "{p25}"),
       median_iqr = c("Médiane (IQR)" = "{median} ({IQR})"),
       q3 = c("Q3" = "{p75}"),
       max = c("Max" = "{max}"),
       mean_sd = c("Moyenne±SD" = "{mean}±{sd}")) |>
    list_modify(!!!qt_stat)

  qt_type <- \(x) {

    list(x$vars$parametric ~ x$stat$mean_sd,
         x$vars$nonparametric ~ x$stat$median_iqr)

  }

  qt_label <- \(x) {

    list(x$vars$parametric ~ str_cap(tolower, names(x$stat$mean_sd)),
         x$vars$nonparametric ~ str_cap(tolower, names(x$stat$median_iqr)))

  }

### QL DATA -----------------------------------------------------------------------------

  ql_vars <-
  data |>
    select(where(~ !is.numeric(.))) |>
    names()

  ql_stat <-
  list(n_pct = c("n (%)" = "{n} ({p})")) |>
    list_modify(!!!ql_stat)

### BIN DATA ----------------------------------------------------------------------------

  bin_vars <-
  data |>
    select(-eval(qt_vars$total), -all_of(ql_vars)) |>
    names()

### ASSIGN ------------------------------------------------------------------------------

  descr <-
  lst(qt =
        lst(vars = qt_vars,
            stat = qt_stat,
            spanner = names(list_c(stat)),
            type = qt_type,
            label = qt_label),
      ql =
        lst(vars = ql_vars,
            stat = ql_stat,
            spanner = names(list_c(stat))),
      bin =
        lst(vars = bin_vars,
            stat = ql_stat,
            spanner = names(list_c(stat))))

### CLI -------------------------------------------------------------------------------

  cli_qt_total_length <-
  data |>
    dplyr::select(eval(descr$qt$vars$total)) |>
    length()

  cli_qt_p <- str_flatten_comma(descr$qt$vars$parametric)
  cli_qt_np <- str_flatten_comma(descr$qt$vars$nonparametric)
  cli_ql <- str_flatten_comma(descr$ql$vars)
  cli_bin <- str_flatten_comma(descr$bin$vars)

  cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli_text("\n\n")
  cli_alert_success("{.strong Quantitative:} {cli_qt_total_length}")
    cli_li(c("Parametric: {cli_qt_p}",
             "Non-parametric: {cli_qt_np}"))
  cli_text("\n\n")
  cli_alert_success("{.strong Qualitative:} {length(descr$ql$vars)}")
    cli_li("{cli_ql}")
  cli_text("\n\n")
  cli_alert_success("{.strong Binary:} {length(descr$bin$vars)}")
    cli_li("{cli_bin}")
  cli_text("\n\n")
  cli_rule()

  return(descr)

}
