#' Title
#'
#' @param data
#' @param fr 
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
                fr = FALSE,
                parametric = NULL,
                qt_stat = NULL,
                ql_stat = NULL) {

  cli_h1("easy_descr")
  cli_text("\n\n")

### QT DATA -------------------------------------------------------------------------------

  qt_vars <-
  lst(total = 
        data |> 
          keep(~ is.numeric(.) & length(unique(na.omit(.))) != 2) |>
          names(),
      parametric = parametric,
      nonparametric = 
        data |> 
          select(all_of(total), -all_of(parametric)) |> 
          names())

  .qt_stat <-
  list(min = c("Min" = "{min}"),
       q1 = c("Q1" = "{p25}"),
       median_iqr = c("Median (IQR)" = "{median} ({p25}—{p75})"),
       q3 = c("Q3" = "{p75}"),
       max = c("Max" = "{max}"),
       mean_sd = c("Mean±SD" = "{mean}±{sd}"))
  
  if (fr) {
   
    .qt_stat <-
    list_modify(.qt_stat,
                median_iqr = c("Médiane (IQR)" = "{median} ({p25}—{p75})"),
                mean_sd = c("Moyenne±SD" = "{mean}±{sd}"))
     
  }
  
  qt_stat <- list_modify(.qt_stat, !!!qt_stat)

### QL DATA -----------------------------------------------------------------------------

  ql_vars <- data |> keep(~ !is.numeric(.) & !is.Date(.)) |> names()

  ql_stat <-
  list(n_pct = c("n (%)" = "{n} ({p})")) |>
    list_modify(!!!ql_stat)

### BIN DATA ----------------------------------------------------------------------------

  bin_vars <-
  data |>
    select(-eval(qt_vars$total), -all_of(ql_vars), -where(is.Date)) |>
    names()

### DATE DATA ---------------------------------------------------------------------------

  date_vars <- data |> keep(is.Date) |> names()

### ASSIGN ------------------------------------------------------------------------------

  descr <-
  lst(qt =
        lst(vars = qt_vars,
            stat = qt_stat,
            spanner = names(list_c(stat))),
      ql =
        lst(vars = ql_vars,
            stat = ql_stat,
            spanner = names(list_c(stat))),
      bin =
        lst(vars = bin_vars,
            stat = ql_stat,
            spanner = names(list_c(stat))),
      date =
        lst(vars = date_vars))

### CLI -------------------------------------------------------------------------------

  cli_qt_total_length <-
  data |>
    select(eval(descr$qt$vars$total)) |>
    length()

  cli_qt_p <- str_flatten_comma(descr$qt$vars$parametric)
  cli_qt_np <- str_flatten_comma(descr$qt$vars$nonparametric)
  cli_ql <- str_flatten_comma(descr$ql$vars)
  cli_bin <- str_flatten_comma(descr$bin$vars)
  cli_date <- str_flatten_comma(descr$date$vars)

  cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli_text("\n\n")
  cli_alert_success("{.strong Quantitative:} {cli_qt_total_length} variables")
    cli_li(c("Parametric: {cli_qt_p}",
             "Non-parametric: {cli_qt_np}"))
  cli_text("\n\n")
  cli_alert_success("{.strong Qualitative:} {length(descr$ql$vars)} variables")
    cli_li("{cli_ql}")
  cli_text("\n\n")
  cli_alert_success("{.strong Dichotomous:} {length(descr$bin$vars)} variables")
    cli_li("{cli_bin}")
  cli_text("\n\n")
  cli_alert_success("{.strong Date:} {length(descr$date$vars)} variables")
    cli_li("{cli_date}")
  cli_text("\n\n")
  cli_rule()

  return(descr)

}
