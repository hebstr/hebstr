#' Automatically analyze the descriptive structure of a dataset
#'
#' Classifies the variables of a dataset by their statistical type and returns
#' a formatted descriptive report. Quantitative (parametric and non-parametric),
#' qualitative, dichotomous and date variables are identified automatically, and
#' the appropriate descriptive statistics are configured for each category. The
#' function supports preliminary data exploration and the preparation of
#' statistical analyses.
#'
#' @param data A data.frame containing the data to analyze. Every column is
#'   examined and classified by its statistical type.
#' @param parametric Regular expression pattern or criterion specifying which
#'   numeric variables should be treated as parametric. Defaults to `nullfile()`
#'   for automatic detection.
#' @param qt_stat Optional list of custom statistics for quantitative variables.
#'   Overrides the default statistics (min, Q1, median, Q3, max,
#'   mean±standard deviation).
#' @param ql_stat Optional list of custom statistics for qualitative variables.
#'   Overrides the default statistic (counts and percentages).
#'
#' @returns A structured list containing the variable classification and its
#'   associated statistics, organized into four main categories. Each category
#'   includes the variable names (`vars`), the configured statistics (`stat`)
#'   and the corresponding column headers (`spanner`).
#'
#' @section Automatic classification:
#' The function applies a sequential classification algorithm that examines each
#' variable against defined statistical criteria. Numeric variables with more
#' than two unique values are classified as quantitative, then split into
#' parametric and non-parametric according to the specified pattern. Non-numeric,
#' non-date variables become qualitative. Binary numeric variables (exactly two
#' values) are classified as dichotomous. Date variables are identified
#' separately for date-specific handling.
#'
#' @section Language adaptation of statistics:
#' The function detects the language configuration via `getOption("OutDec")` and
#' adapts the statistic labels accordingly. Under a French configuration (decimal
#' comma), the terms "Médiane" and "Moyenne" replace their English equivalents.
#' This adaptation applies without manual intervention.
#'
#' @section Formatted CLI report:
#' The function produces a report through the CLI interface presenting the
#' variable classification as a tibble with one row per variable. The tibble
#' reports each variable name, its abbreviated storage type (the `labelled`
#' dictionary code, with numeric variables shown as `num` or `bin`, aligned with
#' [easy_view()]) and its operational statistical type (quantitative parametric,
#' quantitative non-parametric, qualitative categorical, qualitative binary or
#' date). This supports rapid inspection of the data structure and validation of
#' the automatic classification before statistical analysis.
#'
#' @section Technical requirements and dependencies:
#' The function relies on `cli` for output formatting, `labelled` for the
#' variable-type dictionary, `stringr` for string manipulation, `purrr` for list
#' operations, and `glue` for interpolation. The helper functions `nullfile()`,
#' `str_u()` and `list_modify()` must also be available in the execution
#' environment.
#'
#' @examples
#' # Default configuration with automatic classification
#' df_mtcars <- easy_descr(mtcars)
#' str(df_mtcars, max.level = 2)
#' df_mtcars
#'
#' # Accessing elements
#' df_mtcars$qt$vars$total
#' df_mtcars$ql$vars
#'
#' # Explicit specification of parametric variables
#' df_mtcars_para <- mtcars |> easy_descr(parametric = "mpg|hp|disp")
#' df_mtcars_para$qt$vars$parametric
#'
#' # Default typography (EN)
#' df_en <- easy_descr(mtcars)
#' df_en$qt$stat |> with(c(median, mean))
#'
#' # FR typography
#' lang_fr()
#' df_fr <- easy_descr(mtcars)
#' df_fr$qt$stat |> with(c(median, mean))
#'
#' # Customizing quantitative statistics
#' df_stats <-
#' easy_descr(data = mtcars,
#'            qt_stat =
#'              list(median = c("Median" = "{median}"),
#'                   range = c("Range" = "{min}-{max}")))
#'
#' df_stats$qt$stat
#'
#' # Analysis with date data
#' df_storms <-
#' dplyr::storms |>
#'   dplyr::mutate(date_storm = as.Date(paste(year, month, day, sep = "-")))
#'
#' # Configuration treating all continuous variables as parametric
#' df_storms_para <- df_storms |> easy_descr(parametric = "all_continuous")
#' df_storms_para$qt$vars$parametric
#' df_storms_para$qt$vars$nonparametric
#'
#' # Application with gtsummary (TODO)
#'
#' @family exploratory analysis functions
#'
#' @export
easy_descr <- \(
  data,
  parametric = nullfile(),
  qt_stat = NULL,
  ql_stat = NULL
) {
  cli_h1("easy_descr")
  cli_text("\n\n")

  ### QT DATA -------------------------------------------------------------------------------

  str_parametric <- glue("\\b(?:{parametric})\\b")

  qt_vars <- lst(
    total = data |>
      keep(~ is.numeric(.) & length(unique(na.omit(.))) != 2) |>
      names(),
    parametric = str_subset(total, str_u(str_parametric)),
    nonparametric = data |>
      select(all_of(total), -matches(str_parametric)) |>
      names()
  )

  if (all(parametric == "all_continuous")) {
    qt_vars$parametric <- qt_vars$total
    qt_vars$nonparametric <- NULL
  }

  .qt_stat <- list(
    min = c("Min" = "{min}"),
    q1 = c("Q1" = "{p25}"),
    median = c("Median (IQR)" = "{median} ({p25}\u2014{p75})"),
    q3 = c("Q3" = "{p75}"),
    max = c("Max" = "{max}"),
    mean = c("Mean\u00b1SD" = "{mean}\u00b1{sd}")
  )

  if (getOption("OutDec") == ",") {
    .qt_stat <- .qt_stat |>
      list_modify(
        median = c("M\u00e9diane (IQR)" = "{median} ({p25}\u2014{p75})"),
        mean = c("Moyenne\u00b1SD" = "{mean}\u00b1{sd}")
      )
  }

  qt_stat <- .qt_stat |> list_modify(!!!qt_stat)

  ### QL DATA -----------------------------------------------------------------------------

  ql_vars <- data |>
    keep(~ !is.numeric(.) & !is.Date(.)) |>
    names()

  ql_stat <- list(n = c("n (%)" = "{n} ({p})")) |>
    list_modify(!!!ql_stat)

  ### BIN DATA ----------------------------------------------------------------------------

  bin_vars <- data |>
    select(-eval(qt_vars$total), -all_of(ql_vars), -where(is.Date)) |>
    names()

  ### DATE DATA ---------------------------------------------------------------------------

  date_vars <- data |> keep(is.Date) |> names()

  ### ASSIGN ------------------------------------------------------------------------------

  descr <- lst(
    qt = lst(vars = qt_vars, stat = qt_stat, spanner = names(list_c(stat))),
    ql = lst(vars = ql_vars, stat = ql_stat, spanner = names(list_c(stat))),
    bin = lst(vars = bin_vars, stat = ql_stat, spanner = names(list_c(stat))),
    date = lst(vars = date_vars)
  )

  ### CLI -------------------------------------------------------------------------------

  cli_descr <- tibble(
    variable = names(data),
    type = generate_dictionary(data)$col_type,
    group = case_when(
      variable %in% descr$qt$vars$parametric ~ "quanti parametric",
      variable %in% descr$qt$vars$nonparametric ~ "quanti non-parametric",
      variable %in% descr$ql$vars ~ "categorical",
      variable %in% descr$bin$vars ~ "dichotomous",
      variable %in% descr$date$vars ~ "date"
    )
  ) |>
    mutate(
      type = case_when(
        variable %in% descr$bin$vars ~ "bin",
        variable %in% descr$qt$vars$total ~ "num",
        .default = type
      )
    )

  cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli_text("\n\n")
  cli_verbatim(format(cli_descr, n = Inf))
  cli_text("\n\n")
  cli_rule()

  return(descr)
}
