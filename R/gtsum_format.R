.fmt_by <- \(x,
             .check_by,
             .label_header,
             .bold_p) {

  .by <- x$df_by$by_col

  .levels <- labelled::var_label(x$inputs$data[[.check_by]])

  .level_np <-
  "**{level}<br>(n={n}, {gtsummary::style_percent(p, digits = 1)}%)**"

  x <-
  x |>
    gtsummary::add_overall() |>
    gtsummary::modify_table_body(
      ~ . |>
        dplyr::mutate(dplyr::across(dplyr::contains("stat_"),
                      ~ ifelse(stringr::str_starts(., "0.0\\d+"),
                               "—", .)))
    ) |>
    gtsummary::modify_spanning_header(all_of(.by) ~ glue::glue("**{.levels}**")) |>
    gtsummary::modify_header(label ~ .label_header,
                             stat_0 ~ "**Total<br>(N={N})**",
                             all_of(.by) ~ .level_np) |>
    gtsummary::modify_footnote(everything() ~ NA)

  if ("p.value" %in% names(x$table_body)) {

    x <-
    x |>
      gtsummary::modify_header(p.value ~ "**p**") |>
      gtsummary::bold_p(t = .bold_p)

  }

  return(x)

}

.fmt_uni <- \(x,
              .label_header,
              .label_stat) {

  x |>
    gtsummary::modify_header(label ~ .label_header,
                             stat_0 ~ .label_stat) |>
    gtsummary::modify_footnote(everything() ~ NA)

}


.fmt_reg_estim_ci <- \(x,
                       .adj_label,
                       .adj_acro,
                       .estim_label,
                       .ci_label,
                       .ci_data,
                       .estim_sep_int) {

  is_mvreg <- "tbl_regression" %in% class(x)

  .lab <-
  list("OR" ~ "odds ratio",
       "HR" ~ "hazard ratio",
       "Beta" ~ "regression coefficient") |>
    purrr::list_modify(!!!.estim_label)

  x <-
  x |>
    gtsummary::modify_table_body(
      ~ . |>
        dplyr::mutate(coefficients_label =
                        dplyr::case_when(coefficients_label == "exp(Beta)" ~ "OR",
                                         .default = coefficients_label),
                      adj_coefficients_label =
                        dplyr::case_when(is_mvreg ~ glue::glue("{.adj_label}{coefficients_label}"),
                                         .default = coefficients_label),
                      estim_label =
                        dplyr::case_match(coefficients_label,
                                          !!!.lab))
    )

  .coef_label <- unique(x$table_body$adj_coefficients_label)

  x <-
  x |>
    gtsummary::modify_header(estimate ~ glue::glue("**{.coef_label} {.ci_label}**")) |>
    gtsummary::modify_table_styling(columns = estimate,
                                    rows = !is.na(ci),
                                    cols_merge_pattern = paste("{estimate}", .ci_data)) |>
    gtsummary::modify_table_styling(columns = ci,
                                    hide = TRUE)

  estim <-
  list(acro = unique(x$table_body$coefficients_label),
       label = unique(x$table_body$estim_label))

  assign(".estim",
         list(base = glue::glue("{estim$acro}{.estim_sep_int}{estim$label}"),
              ajust = glue::glue("{.adj_label}{estim$acro}{.estim_sep_int}{.adj_acro} {estim$label}")),
         envir = .GlobalEnv)

  return(x)

}


.fmt_reg_level <- \(x,
                    .model_mv,
                    .ref_no) {

  levels <-
  broom.helpers::model_list_terms_levels(.model_mv) |>
    dplyr::select(variable, reference_level) |>
    dplyr::distinct()

  ref_level <- rlang::expr(reference_level %in% c(.ref_no, NA))

  x <-
  x |>
    gtsummary::modify_table_body(
      ~ . |>
        dplyr::left_join(levels, by = "variable") |>
        dplyr::mutate(level =
                        stringr::str_extract(term, glue::glue("(?<={str_u(x$table_body$variable)}).+")),
                      label =
                        dplyr::case_when(var_type == "dichotomous" & !eval(ref_level)
                                         ~ glue::glue("{level} — ref: {reference_level}"),
                                         var_type == "dichotomous" & eval(ref_level)
                                         ~ glue::glue("{label} — ref: {.ref_no}"),
                                         .default = label))
    )

  return(x)

}


.fmt_reg_hide_n <- \(x) {

  .N_event <- unique(x$table_body$N_event)

  if (length(.N_event) == 1) {

    x <-
    x |>
      gtsummary::modify_table_body(
        ~ . |>
          dplyr::mutate(n_event =
                          dplyr::case_when(n_event == N_event
                                           & var_type == "continuous"
                                           ~ NA,
                                           .default = n_event))
      )

    x <- gtsummary::modify_header(x, n_event ~ glue::glue("**n (N={.N_event})**"))

  } else {

    x <-
    x |>
      gtsummary::modify_table_body(
        ~ . |>
          dplyr::mutate(n_event =
                          dplyr::case_when(n_event == N_event ~ NA,
                                           .default = n_event),
                        N_event_uv =
                          dplyr::case_when(var_type == "categorical"
                                           & header_row == FALSE
                                           ~ NA,
                                           .default = N_event))
      ) |>
      gtsummary::modify_header(n_event ~ "**n**",
                               N_event_uv ~ "**N**") |>
      gtsummary::modify_table_body(
        ~ . |>
          dplyr::relocate(N_event_uv,
                          .after = n_event)
      )

  }

  return(x)

}


.fmt_reg <- \(x,
              .adj_label,
              .adj_acro,
              .estim_label,
              .ci,
              .estim_sep_int,
              .model_mv,
              .ref_no,
              .hide_n,
              .label_header,
              .bold_p) {

  x <-
  .fmt_reg_estim_ci(x,
                    .adj_label = .adj_label,
                    .adj_acro = .adj_acro,
                    .estim_label = .estim_label,
                    .ci_label = .ci$label,
                    .ci_data = .ci$data,
                    .estim_sep_int = .estim_sep_int)

  if (!is.null(broom.helpers::model_list_terms_levels(.model_mv))) {

    x <-
    .fmt_reg_level(x,
                   .model_mv = .model_mv,
                   .ref_no = .ref_no)

  }

  if (!.hide_n) x <- .fmt_reg_hide_n(x)

  x <-
  x |>
    gtsummary::modify_header(label ~ .label_header,
                             p.value ~ "**p**") |>
    gtsummary::bold_p(t = .bold_p) |>
    gtsummary::modify_footnote(everything() ~ NA,
                               abbreviation = TRUE)

}


.fmt_indent <- \(x,
                 .vargrp_levels,
                 .indent_type) {

  x |>
    gtsummary::modify_table_body(
      ~ . |>
        dplyr::mutate(row_type =
                        ifelse(variable %in% .vargrp_levels,
                               "level", row_type))
    ) |>
    gtsummary::modify_table_styling(columns = label,
                                    rows = row_type == "level",
                                    text_format = .indent_type)

}


#' Title
#'
#' @param x
#' @param label_header
#' @param label_stat
#' @param bold_p
#' @param adj_label
#' @param adj_acro
#' @param estim_label
#' @param ci
#' @param estim_sep_int
#' @param model_mv
#' @param ref_no
#' @param hide_n
#' @param vargrp_levels
#' @param indent_type
#'
#' @return
#' @export
#'
#' @examples
#'
gtsum_format <- \(x,
                  label_header = "**Variable**",
                  label_stat = "",
                  bold_p = "",
                  adj_label = "a",
                  adj_acro = "adjusted",
                  estim_label = NULL,
                  ci,
                  estim_sep_int,
                  model_mv,
                  ref_no,
                  hide_n = TRUE,
                  vargrp_levels = "",
                  indent_type = "indent") {

  if ("tbl_merge" %in% class(x)) {

    check_by <- x$tbls[[1]]$by

  } else check_by <- x$by

  if (!is.null(check_by)) {

    x <-
    .fmt_by(x,
            .check_by = check_by,
            .label_header = label_header,
            .bold_p = bold_p)

  } else {

    if (stringr::str_detect(class(x)[1], "reg")) {

      x <-
      .fmt_reg(x,
               .adj_label = adj_label,
               .adj_acro = adj_acro,
               .estim_label = estim_label,
               .ci = ci,
               .estim_sep_int = estim_sep_int,
               .model_mv = model_mv,
               .ref_no = ref_no,
               .hide_n = hide_n,
               .label_header = label_header,
               .bold_p = bold_p)

    } else {

      x <-
      .fmt_uni(x,
               .label_header = label_header,
               .label_stat = label_stat)

    }

  }

  x <-
  .fmt_indent(x,
              .vargrp_levels = vargrp_levels,
              .indent_type = indent_type)

}
