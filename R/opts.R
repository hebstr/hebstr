#' Set French locale for R and gtsummary
#'
#' Configures the R environment to use French formatting conventions,
#' including the comma as decimal separator and French language settings
#' for gtsummary tables. Also allows resetting to English defaults.
#'
#' @param reset Logical. If `FALSE` (default), activates French locale.
#'   If `TRUE`, restores English defaults.
#'
#' @returns Called for its side effects (invisible `NULL`). Modifies
#'   `options(OutDec)` and the gtsummary theme.
#' @export
#'
#' @family configuration
#'
#' @seealso [gtsummary::theme_gtsummary_language()]
#'
#' @examples
#' withr::with_options(list(OutDec = "."), {
#'   lang_fr()
#'   getOption("OutDec")
#'
#'   lang_fr(reset = TRUE)
#'   getOption("OutDec")
#' })
#'
lang_fr <- \(reset = FALSE) {
  if (reset) {
    options(OutDec = ".")

    reset_gtsummary_theme()

    cli_alert_info("Setting language: EN")
  } else {
    options(OutDec = ",")

    suppressMessages(
      theme_gtsummary_language(language = "fr", big.mark = " ")
    )

    cli_alert_info("Setting language: FR")
  }

  invisible(NULL)
}

#' Title
#'
#' @param data arg
#' @param .parametric arg
#' @param ... arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
use_vars <- \(data, .parametric = check_opts(parametric), ...) {
  assign(".vars_context", new.env(parent = emptyenv()), envir = globalenv())

  .vars_context$current <- easy_descr(data, .parametric, ...)

  return(data)
}

#' Title
#'
#' @param env arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
clear_vars <- \(env = ".vars_context") {
  if (exists(env, envir = globalenv())) rm(list = env, envir = globalenv())
}


#' Title
#'
#' @param .default_font arg
#' @param .vars_envir arg
#' @param .assign arg
#' @param .name arg
#' @param ... arg
#'
#' @returns arg
#'
#' @export
#' @examples "arg"
#'
set_opts <- \(
  .default_font = "trebuchet ms",
  .vars_envir = .vars_context$current,
  .assign = TRUE,
  .name = "opts",
  ...
) {
  dots <- lst(...)

  .fonts <- \(x) check_fonts(.default = .default_font, .auto = x)

  .label <-
    list(
      n = label_number(accuracy = .1, decimal.mark = getOption("OutDec")),
      p = label_percent(
        accuracy = .1,
        suffix = "",
        decimal.mark = getOption("OutDec")
      )
    )

  .opts_set <-
    lst(
      parametric = nullfile(),
      qt_stat = list(
        min = c("Min" = "{min}"),
        q1 = c("Q1" = "{p25}"),
        median = c("Median (IQR)" = "{median} ({p25}\u2014{p75})"),
        q3 = c("Q3" = "{p75}"),
        max = c("Max" = "{max}"),
        mean = c("Mean\u00b1SD" = "{mean}\u00b1{sd}")
      ),
      ql_stat = list(n = c("n (%)" = "{n} ({p})")),
      labs = list(
        sex = list(m = "Male", f = "Female"),
        bin = list(no = "No", yes = "Yes"),
        missing = "Missing data",
        header = "Characteristic",
        reference = "Reference",
        overall = "Overall",
        spanner = glue("{c('Univariable', 'Multivariable')} analysis")
      ),
      sep = list(int = ": ", ext = "; "),
      ci = list(
        lim = "[",
        sep = "; ",
        label = "95%CI",
        data = c("conf.low", "conf.high")
      ),
      acro = acro(),
      digits = list(
        all_continuous() ~ c(1, .label$n, .label$n),
        all_categorical() ~ c(0, .label$p)
      ),
      pvalue = list(format = label_style_pvalue(digits = 2), seuil = 0.05),
      font = list(alpha = "luciole", digit = "luciole"),
      color = list(
        base = "#999",
        cold = c("#F0FAFF", "#0099FF"),
        warm = c("#FFF5F5", "#FF0000")
      ),
      palette = c(color$base, color$cold[2])
    )

  if (getOption("OutDec") == ",") {
    .opts_set <-
      .opts_set |>
      list_modify(
        qt_stat = list(
          median = c("M\u00e9diane (IQR)" = "{median} ({p25}\u2014{p75})"),
          mean = c("Moyenne\u00b1SD" = "{mean}\u00b1{sd}")
        ),
        labs = list(
          sex = list(m = "Hommes", f = "Femmes"),
          bin = list(no = "Non", yes = "Oui"),
          missing = "Donn\u00e9e manquante",
          header = "Variable",
          reference = "R\u00e9f\u00e9rence",
          overall = "Total",
          spanner = glue("Analyse {c('univariable', 'multivariable')}")
        ),
        sep = list(int = " : ", ext = " ; "),
        ci = list(sep = " ; ", label = "IC95%"),
        acro = acro()
      )
  }

  .vars <- \(..., envir = .vars_envir) {
    cap <- \(x) str_cap(tolower, names(x))

    list(
      test = list(
        envir$qt$vars$parametric ~ "quanti.test.para",
        envir$qt$vars$nonparametric ~ "quanti.test.nonpara",
        all_categorical() ~ "quali.test"
      ),
      stat = list(
        envir$qt$vars$parametric ~ envir$qt$stat$mean,
        envir$qt$vars$nonparametric ~ envir$qt$stat$median,
        all_categorical() ~ envir$ql$stat$n
      ),
      label = list(
        envir$qt$vars$parametric ~ cap(envir$qt$stat$mean),
        envir$qt$vars$nonparametric ~ cap(envir$qt$stat$median),
        all_categorical() ~ cap(envir$ql$stat$n)
      )
    )
  }

  .ci <- \(lim, sep, label, data) {
    lim <- if (lim == "[") c("[", "]") else c("(", ")")

    lst(
      label = glue("{lim[1]}{label}{lim[2]}"),
      data = glue("{lim[1]}{{{data[1]}}}{sep}{{{data[2]}}}{lim[2]}")
    )
  }

  opts <- .opts_set

  opts <-
    opts |>
    list_modify(
      vars = .vars(!!!with(opts, list(parametric, qt_stat, ql_stat))),
      qt_stat_wide = opts$qt_stat |>
        list_modify(
          median = list2(
            "{str_remove(names(opts$qt_stat$median), '\\\\s.+')}" := str_remove(
              unname(opts$qt_stat$median),
              "\\s.+"
            )
          ) |>
            unlist()
        ) |>
        list_c(),
      ci = .ci(!!!opts$ci),
      font = list(
        alpha = .fonts(opts$font[[1]]),
        digit = .fonts(opts$font[[2]])
      ),
      !!!dots
    ) |>
    inject()

  if (identical(opts$font[[1]], opts$font[[2]])) {
    opts$font <- opts$font[[1]]
  }

  if (.assign) {
    assign(.name, opts, envir = .GlobalEnv)
  } else {
    return(get(.name))
  }
}


check_opts <- \(x, .name = "opts") {
  if (exists(.name)) {
    with(get(.name), eval(enexpr(x)))
  } else {
    cli_abort(
      c(
        "{.strong {.name}} does not exist in the global environment.",
        i = "Create it with {.fun set_opts}."
      )
    )
  }
}
