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

#' Cache a dataset's variable classification for `opts$vars`
#'
#' Classifies the variables of `data` with [easy_descr()] and stores the result
#' in a `.vars_context` object in the global environment. The deferred
#' `opts$vars` statistic, test, and label formulas built by [set_opts()] resolve
#' against this cached classification, so `use_vars()` must run on a dataset
#' before those formulas are evaluated. Pipe-friendly: returns `data` unchanged.
#'
#' @param data A data frame whose variables are to be classified.
#' @param .parametric Passed to [easy_descr()] to control the
#'   parametric/non-parametric split. Defaults to the `parametric` option.
#' @param ... Passed on to [easy_descr()].
#'
#' @returns `data`, unchanged (for use in a pipe). Called for its side effect of
#'   setting `.vars_context` in the global environment.
#' @export
#'
#' @family configuration
#'
#' @seealso [set_opts()], [clear_vars()]
#'
#' @examples
#' set_opts()
#' use_vars(mtcars)
#' clear_vars()
#'
use_vars <- \(data, .parametric = check_opts(parametric), ...) {
  assign(".vars_context", new.env(parent = emptyenv()), envir = globalenv())

  .vars_context$current <- easy_descr(data, .parametric, ...)

  return(data)
}

#' Remove the cached variable classification
#'
#' Removes the `.vars_context` object created by [use_vars()] from the global
#' environment. No-op when the object is absent.
#'
#' @param env Name of the object to remove from the global environment.
#'   Defaults to `".vars_context"`.
#'
#' @returns Called for its side effect (invisible `NULL`).
#' @export
#'
#' @family configuration
#'
#' @seealso [use_vars()], [set_opts()]
#'
#' @examples
#' set_opts()
#' use_vars(mtcars)
#' clear_vars()
#'
clear_vars <- \(env = ".vars_context") {
  if (exists(env, envir = globalenv())) rm(list = env, envir = globalenv())
}


#' Build the package options object
#'
#' Assembles the centralised options list consumed across the package: statistic
#' templates, table labels, separators, confidence-interval formatting, acronym
#' dictionary, fonts, and colours. Labels and formats follow the locale set by
#' [lang_fr()] (comma decimal mark selects French wording). The result is either
#' assigned to a named object in the global environment (the default) or returned
#' for inspection. Keys can be overridden through `...`.
#'
#' @param .default_font Font family used for both the text (`alpha`) and numeric
#'   (`digit`) font slots unless overridden through `font`. Passed through
#'   [check_fonts()], so an unavailable font falls back to the OS-agnostic
#'   system sans-serif (`"sans"`). Defaults to `"sans"`.
#' @param .vars_envir Optional variable classification (as produced by
#'   [easy_descr()]) used to resolve the deferred `opts$vars` formulas. When
#'   `NULL` (default), the formulas resolve against the `.vars_context` cached by
#'   [use_vars()] at evaluation time.
#' @param .assign Logical. If `TRUE` (default), the options object is assigned to
#'   `.name` in the global environment; if `FALSE`, it is returned.
#' @param .name Name under which the options object is stored in, and retrieved
#'   from, the global environment. Defaults to `"opts"`.
#' @param ... Named overrides for any option key. Names are validated against the
#'   set of valid keys; an unknown name aborts.
#'
#' @returns The options list. With `.assign = TRUE` (default) it is also assigned
#'   to `.name` in the global environment.
#'
#' @export
#'
#' @family configuration
#'
#' @seealso [use_vars()], [check_opts()], [lang_fr()]
#'
#' @examples
#' opts <- set_opts(.assign = FALSE)
#' opts$sep$ext
#'
set_opts <- \(
  .default_font = "sans",
  .vars_envir = NULL,
  .assign = TRUE,
  .name = "opts",
  ...
) {
  if (
    !.assign &&
      ...length() == 0L &&
      missing(.default_font) &&
      is.null(.vars_envir) &&
      exists(.name, envir = .GlobalEnv)
  ) {
    return(get(.name, envir = .GlobalEnv))
  }

  dots <- lst(...)

  user_vars_envir <- .vars_envir
  last_seen_envir <- NULL
  resolve_vars_envir <- \() {
    if (!is.null(user_vars_envir)) {
      return(user_vars_envir)
    }
    if (
      exists(".vars_context", envir = globalenv()) &&
        !is.null(get(".vars_context", envir = globalenv())$current)
    ) {
      last_seen_envir <<- get(".vars_context", envir = globalenv())$current
      return(last_seen_envir)
    }
    if (!is.null(last_seen_envir)) {
      return(last_seen_envir)
    }
    cli_abort(
      c(
        "{.code .vars_context$current} is not set.",
        i = "Call {.fun use_vars} on your dataset before evaluating {.code opts$vars$*}, or pass {.arg .vars_envir} explicitly to {.fun set_opts}."
      )
    )
  }

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

  valid_opts <- c(names(.opts_set), "vars", "qt_stat_wide")
  unknown_opts <- setdiff(names(dots), valid_opts)
  if (length(unknown_opts) > 0) {
    cli_abort(
      c(
        "Invalid option names in {.arg ...}: {.field {unknown_opts}}.",
        i = "Valid names: {.field {valid_opts}}."
      )
    )
  }

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

  .vars <- \() {
    cap <- \(x) str_cap(tolower, names(x))
    e <- \() resolve_vars_envir()

    list(
      test = list(
        e()$qt$vars$parametric ~ "quanti.test.para",
        e()$qt$vars$nonparametric ~ "quanti.test.nonpara",
        all_categorical() ~ "quali.test"
      ),
      stat = list(
        e()$qt$vars$parametric ~ e()$qt$stat$mean,
        e()$qt$vars$nonparametric ~ e()$qt$stat$median,
        all_categorical() ~ e()$ql$stat$n
      ),
      label = list(
        e()$qt$vars$parametric ~ cap(e()$qt$stat$mean),
        e()$qt$vars$nonparametric ~ cap(e()$qt$stat$median),
        all_categorical() ~ cap(e()$ql$stat$n)
      )
    )
  }

  .ci <- \(ci) {
    lim <- if (ci$lim == "[") c("[", "]") else c("(", ")")

    lst(
      label = glue("{lim[1]}{ci$label}{lim[2]}"),
      data = glue("{lim[1]}{{{ci$data[1]}}}{ci$sep}{{{ci$data[2]}}}{lim[2]}")
    )
  }

  .qt_stat_wide <- \(qt_stat) {
    qt_stat |>
      list_modify(
        median = set_names(
          str_remove(qt_stat$median, "\\s.+"),
          str_remove(names(qt_stat$median), "\\s.+")
        )
      ) |>
      list_c()
  }

  .font <- \(x) {
    x <- as.list(x)
    alpha <- x$alpha %||% x[[1]]
    digit <- x$digit %||% x[[if (length(x) >= 2) 2 else 1]]

    list(alpha = .fonts(alpha), digit = .fonts(digit))
  }

  opts <- .opts_set

  opts <-
    opts |>
    list_modify(
      vars = .vars(),
      qt_stat_wide = .qt_stat_wide(opts$qt_stat),
      ci = .ci(opts$ci),
      !!!dots
    )

  opts$font <- .font(opts$font)

  if (.assign) {
    assign(.name, opts, envir = .GlobalEnv)
  } else {
    return(opts)
  }
}


#' Read a validated key from the package options
#'
#' Reader companion to [set_opts()]: retrieves a value from the centralised
#' options object stored in the global environment, validating that the object
#' exists and that the requested key is one of its keys. Used as the default for
#' arguments across the package (e.g. `acro_list = check_opts(acro)`), and
#' available for direct inspection of the active options.
#'
#' @param x An option key given as an unquoted expression navigating the options
#'   object, e.g. `sep$ext`, `acro`, or `font$alpha`. The root name (`sep`,
#'   `acro`, `font`, ...) is validated against the keys of the options object; an
#'   unknown root aborts.
#' @param .name Name under which the options object is stored in the global
#'   environment, as set by [set_opts()]. Defaults to `"opts"`.
#'
#' @returns The value stored at the requested key of the options object.
#'
#' @export
#'
#' @family configuration
#'
#' @seealso [set_opts()], [use_vars()], [lang_fr()]
#'
#' @examples
#' set_opts()
#' check_opts(sep$ext)
#'
check_opts <- \(x, .name = "opts") {
  if (!exists(.name, envir = .GlobalEnv, inherits = FALSE)) {
    cli_abort(
      c(
        "{.strong { .name }} does not exist in the global environment.",
        i = "Create it with {.fun set_opts}."
      )
    )
  }

  opts <- get(.name, envir = .GlobalEnv, inherits = FALSE)
  key <- enexpr(x)

  root <- key
  while (is.call(root) && length(root) >= 2) {
    root <- root[[2]]
  }

  if (!is_symbol(root) || !as_string(root) %in% names(opts)) {
    cli_abort(
      c(
        "{.field {as_label(root)}} is not a key of {.strong { .name }}.",
        i = "Available keys: {.field {names(opts)}}."
      )
    )
  }

  eval(key, opts)
}
