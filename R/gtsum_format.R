.estim_channel <- new.env(parent = emptyenv())

.fmt_by <- \(x, .check_by, .label_header, .label_overall, .bold_p) {
  .by <-
    lst(
      cols = names(x$table_body) |>
        str_subset("^stat_\\d+$") |>
        setdiff("stat_0"),
      name = x$inputs$data[[.check_by]],
      N = length(na.omit(name)),
      spanner = var_label(name) %||% .check_by
    )

  .levels <- "**{level}<br>(n={n}, {style_percent(p, digits = 1)}%)**"

  x <-
    x |>
    add_overall(col_label = glue("**{.label_overall}<br>(N={.by$N})**")) |>
    modify_spanning_header(all_of(.by$cols) ~ glue("**{.by$spanner}**")) |>
    modify_header(label ~ .label_header, all_of(.by$cols) ~ .levels) |>
    modify_footnote(everything() ~ NA)

  if ("p.value" %in% names(x$table_body)) {
    x <-
      x |>
      modify_header(p.value ~ "**p**") |>
      bold_p(t = .bold_p)
  }

  x
}

.fmt_uni <- \(x, .label_header, .label_stat) {
  x |>
    modify_header(label ~ .label_header, stat_0 ~ .label_stat) |>
    modify_footnote(everything() ~ NA)
}


.fmt_reg_estim_ci <- \(
  x,
  .adj_acro,
  .adj_label,
  .estim_acro,
  .estim_label,
  .ci_label,
  .ci_data,
  .estim_sep
) {
  is_mvreg <- inherits(x, "tbl_regression")

  .lab_test <- list(
    beta = "Beta",
    or = "OR",
    hr = "HR"
  ) |>
    list_modify(!!!.estim_acro)

  .lab <- list(
    beta = "regression coefficient",
    or = "odds ratio",
    hr = "hazard ratio"
  ) |>
    list_modify(!!!.estim_label)

  x <- x |>
    modify_table_body(
      ~ . |>
        mutate(
          coefficients_label = recode_values(
            coefficients_label,
            "Beta" ~ .lab_test$beta,
            "exp(Beta)" ~ .lab_test$or,
            "HR" ~ .lab_test$hr,
            default = coefficients_label
          ),
          adj_coefficients_label = if (is_mvreg) {
            glue("{.adj_acro}{coefficients_label}")
          } else {
            coefficients_label
          },
          estim_label = recode_values(
            coefficients_label,
            .lab_test$beta ~ .lab$beta,
            .lab_test$or ~ .lab$or,
            .lab_test$hr ~ .lab$hr
          )
        )
    )

  .coef_label <- unique(x$table_body$adj_coefficients_label)

  x <- x |>
    modify_header(estimate ~ glue("**{.coef_label} {.ci_label}**")) |>
    modify_column_merge(
      pattern = paste("{estimate}", .ci_data),
      rows = !is.na(estimate)
    )

  estim <- lst(
    acro = unique(x$table_body$coefficients_label),
    adj_acro = glue("{.adj_acro}{acro}"),
    label = unique(x$table_body$estim_label),
    adj_label = glue(.adj_label),
    str = lst(
      uv = if (!str_detect(acro, "^[:upper:]+$")) {
        NULL
      } else {
        glue("{acro}{.estim_sep}{label}")
      },
      mv = if (str_detect(adj_acro, "\\s+") || .adj_acro == "") {
        NULL
      } else {
        glue("{adj_acro}{.estim_sep}{adj_label}")
      }
    )
  )

  .estim_channel$estim <- list(uv = estim$str$uv, mv = estim$str$mv)

  x
}


.fmt_reg_level <- \(x, .model_mv, .ref_sep, .ref_no) {
  levels <- .model_mv |>
    model_list_terms_levels() |>
    select(variable, reference_level) |>
    distinct()

  ref_level <- expr(reference_level %in% c(.ref_no, NA))

  x <- x |>
    modify_table_body(
      ~ . |>
        left_join(levels, by = "variable") |>
        mutate(
          level = str_extract(
            term,
            glue("(?<={str_u(x$table_body$variable)}).+")
          ),
          label = case_when(
            var_type == "dichotomous" & !eval(ref_level) ~ glue(
              "{level} \u2014 ref{.ref_sep}{reference_level}"
            ),
            var_type == "dichotomous" & eval(ref_level) ~ glue(
              "{label} \u2014 ref{.ref_sep}{.ref_no}"
            ),
            .default = label
          )
        )
    )

  x
}

.event <- \(x) "n_event" %in% names(x$table_body)

.fmt_reg_n <- \(x, .stat_n, .label_n) {
  .n_type <- if (.event(x)) "n_event" else "n_obs"

  x <- x |>
    modify_table_body(
      ~ . |>
        mutate(
          stat_n = if_else(
            var_type == "continuous" | row_type == "level",
            glue(.stat_n),
            NA
          ),
          .after = all_of(.n_type)
        )
    ) |>
    modify_header(stat_n = glue("**{.label_n}**"))

  x
}


.fmt_reg_no_event <- \(x) {
  x |>
    modify_table_body(
      ~ . |>
        mutate(across(c(estimate, ci), \(col) if_else(n_event %in% 0, NA, col)))
    )
}


.fmt_reg <- \(
  x,
  .adj_acro,
  .adj_label,
  .estim_acro,
  .estim_label,
  .ci,
  .estim_sep,
  .model_mv,
  .show_single_row,
  .ref_sep,
  .ref_no,
  .stat_n,
  .label_n,
  .label_header,
  .label_reference,
  .bold_p
) {
  x <-
    .fmt_reg_estim_ci(
      x,
      .adj_acro = .adj_acro,
      .adj_label = .adj_label,
      .estim_acro = .estim_acro,
      .estim_label = .estim_label,
      .ci_label = .ci$label,
      .ci_data = .ci$data,
      .estim_sep = .estim_sep
    )

  if (.show_single_row) {
    if (is.null(.model_mv)) {
      cli_abort(
        c(
          "{.arg model_mv} must be supplied when {.arg show_single_row} is {.code TRUE}.",
          i = "{.fun gtsum_format} reads factor reference levels from the fitted model."
        )
      )
    }

    if (!is.null(model_list_terms_levels(.model_mv))) {
      x <-
        .fmt_reg_level(
          x,
          .model_mv = .model_mv,
          .ref_sep = .ref_sep,
          .ref_no = .ref_no
        )
    }
  }

  if ("n_obs" %in% names(x$table_body)) {
    x <- .fmt_reg_n(x, .stat_n, .label_n)
  }

  if (.event(x)) {
    x <- .fmt_reg_no_event(x)
  }

  x |>
    modify_header(label ~ .label_header, p.value ~ "**p**") |>
    add_ref_label(label = .label_reference) |>
    bold_p(t = .bold_p) |>
    modify_footnote(everything() ~ NA, abbreviation = TRUE)
}


.fmt_indent <- \(x, .vargrp_levels, .indent) {
  x |>
    modify_table_body(
      ~ . |>
        mutate(
          row_type = ifelse(variable %in% .vargrp_levels, "level", row_type)
        )
    ) |>
    modify_table_styling(
      columns = label,
      rows = row_type == "level",
      indent = .indent
    )
}

#' Standardise a gtsummary table before styling
#'
#' Applies the package's header, label, and footnote conventions to a
#' `gtsummary` table, dispatching on its type: by-group summaries gain an
#' overall column and level spanners, univariate summaries get a single-stat
#' header, and regression tables get merged estimator/confidence-interval
#' columns, reference-level annotations, and (when the table carries an
#' observation count) an observation/event count column. The estimator
#' definitions are recorded for [gt_format()] to render as a footnote.
#' Typically piped into [gt_format()].
#'
#' When a regression table carries an event count, levels with no event are
#' blanked: their estimate is not identifiable (the coefficient diverges), so
#' the fitted value and its confidence interval are dropped rather than
#' rendered. Tables without an event count, such as linear regressions, are
#' left untouched.
#'
#' @param x A `gtsummary` table: [gtsummary::tbl_summary()] (optionally with a
#'   `by` group or `add_p()`), [gtsummary::tbl_regression()], or
#'   [gtsummary::tbl_uvregression()].
#' @param label_header Header label for the characteristic (`label`) column.
#'   Defaults to the `labs$header` option. `NULL` leaves it blank.
#' @param label_overall Label for the overall column added to by-group
#'   summaries. Defaults to the `labs$overall` option.
#' @param label_reference Label placed on reference rows of regression tables.
#'   Defaults to the `labs$reference` option.
#' @param label_n Header for the count column of regressions.
#'   Defaults to `"n/N"` when the model carries events, else `"N"`.
#' @param stat_n Glue template for the count column values. Defaults to
#'   `"{n_event}/{n_obs}"` when events are present, else `"{n_obs}"`.
#' @param label_stat Label for the single statistic column (`stat_0`) of a
#'   univariate summary. `NULL` (default) leaves it blank.
#' @param bold_p P-value threshold below which p-values are bolded. Defaults to
#'   `0` (no bolding).
#' @param adj_acro Acronym prefix marking the adjusted (multivariable)
#'   estimator. Defaults to `"a"`.
#' @param adj_label Glue template for the adjusted estimator definition.
#'   Defaults to a bilingual template keyed on `getOption("OutDec")`.
#' @param estim_acro Named list overriding the default estimator acronyms
#'   (`beta`, `or`, `hr`). `NULL` (default) keeps the defaults.
#' @param estim_label Named list overriding the default estimator definitions
#'   (`beta`, `or`, `hr`). `NULL` (default) keeps the defaults.
#' @param ci Confidence-interval specification (`label` and `data` pattern).
#'   Defaults to the `ci` option.
#' @param model_mv The fitted multivariable model, used to read factor
#'   reference levels. Required when `show_single_row` is `TRUE`.
#' @param show_single_row Whether to annotate dichotomous variables with their
#'   reference level, read from `model_mv`. Defaults to `FALSE`.
#' @param ref_sep Separator inserted before the reference-level value in the
#'   annotation. Defaults to the `sep$int` option.
#' @param ref_no Placeholder for an absent reference level. Defaults to `""`.
#' @param estim_sep Separator between acronym and definition in the estimator
#'   footnote. Defaults to the `sep$int` option.
#' @param vargrp_levels Character vector of variable names whose rows are forced
#'   to render as indented levels. Defaults to `""`.
#' @param indent Indentation width, in points, applied to level rows. Defaults
#'   to `4`.
#'
#' @returns The formatted `gtsummary` table, ready for [gt_format()].
#'
#' @examples
#' set_opts()
#' tbl <- gtsummary::tbl_summary(mtcars, include = c(mpg, cyl))
#' gtsum_format(tbl, label_stat = "Overall")
#'
#' @export
#'
gtsum_format <- \(
  x,
  label_header = check_opts(labs$header),
  label_overall = check_opts(labs$overall),
  label_reference = check_opts(labs$reference),
  label_n = if (.event(x)) "n/N" else "N",
  stat_n = if (.event(x)) "{n_event}/{n_obs}" else "{n_obs}",
  label_stat = NULL,
  bold_p = 0,
  adj_acro = "a",
  adj_label = if (getOption("OutDec") == ".") {
    "adjusted {label}"
  } else {
    "{label} ajust\u00e9"
  },
  estim_acro = NULL,
  estim_label = NULL,
  ci = check_opts(ci),
  model_mv = NULL,
  show_single_row = FALSE,
  ref_sep = check_opts(sep$int),
  ref_no = "",
  estim_sep = check_opts(sep$int),
  vargrp_levels = "",
  indent = 4
) {
  clear_vars()

  label_header <- if (!is.null(label_header)) glue("**{label_header}**") else ""
  label_stat <- if (!is.null(label_stat)) glue("**{label_stat}**") else ""

  check_by <- x$inputs$by

  if (length(check_by) > 0) {
    x <- .fmt_by(
      x,
      .check_by = check_by,
      .label_header = label_header,
      .label_overall = label_overall,
      .bold_p = bold_p
    )
  } else {
    if (inherits(x, c("tbl_regression", "tbl_uvregression"))) {
      x <- .fmt_reg(
        x,
        .adj_acro = adj_acro,
        .adj_label = adj_label,
        .estim_acro = estim_acro,
        .estim_label = estim_label,
        .ci = ci,
        .estim_sep = estim_sep,
        .model_mv = model_mv,
        .show_single_row = show_single_row,
        .ref_sep = ref_sep,
        .ref_no = ref_no,
        .stat_n = stat_n,
        .label_n = label_n,
        .label_header = label_header,
        .label_reference = label_reference,
        .bold_p = bold_p
      )
    } else {
      x <- .fmt_uni(x, .label_header = label_header, .label_stat = label_stat)
    }
  }

  .fmt_indent(x, .vargrp_levels = vargrp_levels, .indent = indent)
}
