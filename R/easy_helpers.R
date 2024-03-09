#' Title
#'
#' @param ... A character vector
#' @param replace Replacement pattern. A character vector.
#'
#' @return A named character vector
#' @export
#'
#' @examples
#'
easy_replace <- \(..., replace = "</>") {

  col_replace <- cli::col_br_red(replace)
  col_replace <- glue::glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  purrr::map(c(...),
             ~ rlang::list2('{glue::glue("<p>.*({.}).*</p>")}' := replace) |>
               unlist())

  replace_list <-
    rlang::list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))

}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
easy_recode <- \(...) {

  name_label <- list(...)

  cols <-
  list(name = purrr::map(name_label, ~ .[1]),
       label = purrr::map(name_label, ~ .[2]))

}


#' Title
#'
#' @param x
#' @param var
#' @param inf
#' @param sup
#' @param cut
#' @param .btw
#' @param .name
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
easy_fct <- \(x,
              var,
              inf = NULL,
              sup = NULL,
              cut = NULL,
              .btw = NULL,
              .name = rlang::enexpr(var),
              ...) {

  var <- rlang::enexpr(var)

  if (!is.numeric(x[[var]])) {

    dots <- rlang::exprs(...)

    x |>
      dplyr::mutate(!!var :=
                      rlang::inject(forcats::fct_recode(!!var, !!!dots)) |>
                      forcats::fct_relevel(names(dots)))

  } else {

    lv <-
    list(cut = c(glue::glue("<{cut}"), glue::glue(">={cut}")),
         inf = glue::glue("<{inf}"),
         btw_inf = glue::glue("[{inf}-{.btw})"),
         btw = glue::glue("[{inf}-{sup})"),
         btw_sup = glue::glue("[{.btw}-{sup})"),
         sup = glue::glue(">={sup}"))

    if (is.null(cut)) {

      if (is.null(.btw)) {

        cond <-
        rlang::exprs(!!var < inf ~ lv$inf,
                     !!var >= inf & !!var < sup ~ lv$btw,
                     !!var >= sup ~ lv$sup)

        lvs <- c(lv$inf, lv$btw, lv$sup)

      } else {

        cond <-
        rlang::exprs(!!var < inf ~ lv$inf,
                     !!var >= inf & !!var < .btw ~ lv$btw_inf,
                     !!var >= .btw & !!var < sup ~ lv$btw_sup,
                     !!var >= sup ~ lv$sup)

        lvs <- c(lv$inf, lv$btw_inf, lv$btw_sup, lv$sup)

      }

        x |>
          dplyr::mutate(!!.name :=
                          rlang::inject(dplyr::case_when(!!!cond)) |>
                          forcats::fct_relevel(lvs))

    } else {

      x |>
        dplyr::mutate(!!.name :=
                 cut(!!var,
                     breaks = c(0, cut, Inf),
                     labels = lv$cut,
                     right = FALSE,
                     ...))

    }

  }

}


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
easy_descript <- \(data,
                   parametric = NULL) {

  cli::cli_h1("easy_descript")
  cli::cli_text("\n\n")

### DATA -------------------------------------------------------------------------------

  nonparametric <-
  data |>
    dplyr::select(c(eval(total), -dplyr::all_of(parametric))) |>
    names() |>
    dplyr::expr()

  ql_vars <-
  data |>
    dplyr::select(where(~ !is.numeric(.))) |>
    names()

  qt_stat <- list(min = c("Min" = "{min}"),
                  q1 = c("Q1" = "{p25}"),
                  median_iqr = c("Médiane (IQR)" = "{median} ({IQR})"),
                  q3 = c("Q3" = "{p75}"),
                  max = c("Max" = "{max}"),
                  mean_sd = c("Moyenne±SD" = "{mean}±{sd}"))

  ql_stat <- list(n_pct = c("n (%)" = "{n} ({p})"))

### LIST ------------------------------------------------------------------------------

  descript <-
  dplyr::lst(qt = dplyr::lst(vars = dplyr::lst(total = dplyr::expr(dplyr::where(is.numeric)),
                                               parametric = parametric,
                                               nonparametric = eval(nonparametric)),
                             stat = qt_stat,
                             spanner = names(purrr::list_c(stat))),
             ql = dplyr::lst(vars = eval(ql_vars),
                             stat = ql_stat,
                             spanner = names(purrr::list_c(stat))))

  cli_qt_total_length <-
  data |>
    dplyr::select(eval(descript$qt$vars$total)) |>
    length()

  cli_qt_p <- stringr::str_flatten_comma(descript$qt$vars$parametric)
  cli_qt_np <- stringr::str_flatten_comma(descript$qt$vars$nonparametric)
  cli_ql <- stringr::str_flatten_comma(descript$ql$vars)

### CLI -------------------------------------------------------------------------------

  cli::cli_alert_info("{.strong {substitute(data)}}: {length(data)} variables")
  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong Quantitative:} {cli_qt_total_length}")
    cli::cli_li(c("Parametric: {cli_qt_p}",
                  "Non-parametric: {cli_qt_np}"))
  cli::cli_text("\n\n")
  cli::cli_alert_success("{.strong Qualitative:} {length(descript$ql$vars)}")
    cli::cli_li("{cli_ql}")
  cli::cli_text("\n\n")
  cli::cli_rule()

  return(descript)

}


#' Title
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#'
pca_var_extract <- \(x) {

dplyr::lst(coord =
             x |>
             broom::tidy("rotation") |>
             tidyr::pivot_wider(names_from = "PC",
                                names_prefix = "PC",
                                values_from = "value") |>
             dplyr::mutate(column = stringr::str_remove_all(column, "hamd"),
                           .keep = "all"),
           contrib =
             coord |>
               dplyr::mutate(dplyr::across(dplyr::matches("PC"),
                                           ~ . ^ 2 / sum(. ^ 2))),
           weight =
             coord["column"] |>
               dplyr::mutate(PC1 = contrib$PC1 / max(contrib$PC1)) |>
               pull(PC1))

}


#' Title
#'
#' @param data
#' @param times
#' @param method
#' @param fit
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
easy_boot <- \(data,
               times = 1000,
               method,
               fit,
               ...) {

  .f <- \(.) {

    do.call(method,
            list(parsnip::fit,
                 rsample::analysis(.),
                 ...))

  }

  boot <-
  data |>
    rsample::bootstraps(times = times,
                        apparent = TRUE) |>
    dplyr::mutate(model = purrr::map(splits, .f))

  boot <-
  list(estim_data = tidy,
       fitted = augment) |>
    map(~ boot |>
          dplyr::mutate(coef = purrr::map(model, .)))

  boot <-
  list(estimate =
         list(data = boot$estim_data,
              int = boot$estim_data |> int_pctl(coef)),
       fitted = boot$fitted)

  assign("boot", boot, env = .GlobalEnv)

  print(boot$estim$int)

}
