.fmt_by <- \(x,
             .check_by,
             .label_header,
             .label_overall,
             .bold_p) {

  .by <-
  lst(cols = x$df_by$by_col,
      name = x$inputs$data[[.check_by]],
      spanner = var_label(name) %||% .check_by)

  .levels <- "**{level}<br>(n={n}, {style_percent(p, digits = 1)}%)**"

  x <-
  x |>
    add_overall() |>
    modify_table_body(
      ~ . |>
        mutate(across(contains("stat_"),
                      ~ ifelse(str_starts(., "0.0\\d+"), "—", .)))
    ) |>
    modify_spanning_header(all_of(.by$cols) ~ glue("**{.by$spanner}**")) |>
    modify_header(label ~ .label_header,
                  stat_0 ~ glue("**{.label_overall}<br>(N={x$N})**"),
                  all_of(.by$cols) ~ .levels) |>
    modify_footnote(everything() ~ NA)

  if ("p.value" %in% names(x$table_body)) {

    x <-
    x |>
      modify_header(p.value ~ "**p**") |>
      bold_p(t = .bold_p)

  }

  return(x)

}

.fmt_uni <- \(x,
              .label_header,
              .label_stat) {

  x |>
    modify_header(label ~ .label_header,
                  stat_0 ~ .label_stat) |>
    modify_footnote(everything() ~ NA)

}


.fmt_reg_estim_ci <- \(x,
                       .adj_acro,
                       .adj_label,
                       .estim_acro,
                       .estim_label,
                       .ci_label,
                       .ci_data,
                       .estim_sep) {

  is_mvreg <- "tbl_regression" %in% class(x)

  .lab_test <-
  list(beta = "Beta",
       or = "OR",
       hr = "HR") |> 
    list_modify(!!!.estim_acro)
  
  .lab <-
  list(beta = "regression coefficient",
       or = "odds ratio",
       hr = "hazard ratio") |>
    list_modify(!!!.estim_label)

  x <-
  x |>
    modify_table_body(
      ~ . |>
        mutate(coefficients_label =
                 case_match(coefficients_label,
                            "Beta" ~ .lab_test$beta,
                            "exp(Beta)" ~ .lab_test$or,
                            "HR" ~ .lab_test$hr,
                           .default = coefficients_label),
               adj_coefficients_label =
                 case_when(is_mvreg ~ glue("{.adj_acro}{coefficients_label}"),
                           .default = coefficients_label),
               estim_label =
                 case_match(coefficients_label,
                            .lab_test$beta ~ .lab$beta,
                            .lab_test$or ~ .lab$or,
                            .lab_test$hr ~ .lab$hr))
    )

  .coef_label <- unique(x$table_body$adj_coefficients_label)

  x <-
  x |>
    modify_header(estimate ~ glue("**{.coef_label} {.ci_label}**")) |>
    modify_table_styling(columns = estimate,
                         rows = !is.na(ci),
                         cols_merge_pattern = paste("{estimate}", .ci_data)) |>
    modify_table_styling(columns = ci,
                         hide = TRUE)

  estim <-
  lst(acro = unique(x$table_body$coefficients_label),
      adj_acro = glue("{.adj_acro}{acro}"),
      label = unique(x$table_body$estim_label),
      adj_label = glue(.adj_label),
      str =
        lst(uv =
              if (!str_detect(acro, "^[:upper:]+$")) NULL
              else glue("{acro}{.estim_sep}{label}"),
            mv =
              if (str_detect(adj_acro, "\\s+") | .adj_acro == "") NULL
              else glue("{adj_acro}{.estim_sep}{adj_label}")))

  assign(".estim",
         list(uv = estim$str$uv,
              mv = estim$str$mv),
         envir = .GlobalEnv)

  return(x)

}


.fmt_reg_level <- \(x,
                    .model_mv,
                    .ref_sep,
                    .ref_no) {

  levels <-
  model_list_terms_levels(.model_mv) |>
    select(variable, reference_level) |>
    distinct()

  ref_level <- expr(reference_level %in% c(.ref_no, NA))

  x <-
  x |>
    modify_table_body(
      ~ . |>
        left_join(levels, by = "variable") |>
        mutate(level =
                 str_extract(term, glue("(?<={str_u(x$table_body$variable)}).+")),
               label =
                 case_when(var_type == "dichotomous" & !eval(ref_level)
                           ~ glue("{level} — ref{.ref_sep}{reference_level}"),
                           var_type == "dichotomous" & eval(ref_level)
                           ~ glue("{label} — ref{.ref_sep}{.ref_no}"),
                           .default = label))
    )

  return(x)

}


.fmt_reg_hide_n <- \(x) {

  .N_event <- unique(x$table_body$N_event)

  if (length(.N_event) == 1) {

    x <-
    x |>
      modify_table_body(
        ~ . |>
          mutate(n_event =
                   case_when(n_event == N_event & var_type == "continuous" ~ NA,
                             .default = n_event))
      )

    x <- modify_header(x, n_event ~ glue("**n (N={.N_event})**"))

  } else {

    x <-
    x |>
      modify_table_body(
        ~ . |>
          mutate(n_event =
                   case_when(n_event == N_event ~ NA,
                             .default = n_event),
                 N_event_uv =
                   case_when(var_type == "categorical" & header_row == FALSE ~ NA,
                             .default = N_event))
      ) |>
      modify_header(n_event ~ "**n**",
                    N_event_uv ~ "**N**") |>
      modify_table_body(
        ~ . |>
          relocate(N_event_uv, .after = n_event)
      )

  }

  return(x)

}


.fmt_reg <- \(x,
              .adj_acro,
              .adj_label,
              .estim_acro,
              .estim_label,
              .ci,
              .estim_sep,
              .model_mv,
              .ref_sep,
              .ref_no,
              .hide_n,
              .label_header,
              .bold_p) {

  x <-
  .fmt_reg_estim_ci(x,
                    .adj_acro = .adj_acro,
                    .adj_label = .adj_label,
                    .estim_acro = .estim_acro,
                    .estim_label = .estim_label,
                    .ci_label = .ci$label,
                    .ci_data = .ci$data,
                    .estim_sep = .estim_sep)

  if (!is.null(model_list_terms_levels(.model_mv))) {

    x <-
    .fmt_reg_level(x,
                   .model_mv = .model_mv,
                   .ref_sep = .ref_sep,
                   .ref_no = .ref_no)

  }

  if (!.hide_n) x <- .fmt_reg_hide_n(x)

  x <-
  x |>
    modify_header(label ~ .label_header,
                  p.value ~ "**p**") |>
    bold_p(t = .bold_p) |>
    modify_footnote(everything() ~ NA,
                    abbreviation = TRUE)

}


.fmt_indent <- \(x,
                 .vargrp_levels,
                 .indent_type) {

  x |>
    modify_table_body(
      ~ . |>
        mutate(row_type = ifelse(variable %in% .vargrp_levels, "level", row_type))
    ) |>
    modify_table_styling(columns = label,
                         rows = row_type == "level",
                         text_format = .indent_type)

}


#' Title
#'
#' @param x
#' @param label_header
#' @param label_overall 
#' @param label_stat
#' @param bold_p
#' @param adj_acro
#' @param adj_label
#' @param estim_acro 
#' @param estim_label
#' @param ci
#' @param model_mv
#' @param ref_sep 
#' @param ref_no
#' @param estim_sep
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
                  label_header = NULL,
                  label_overall = "Total",
                  label_stat = NULL,
                  bold_p = "",
                  adj_acro = "a",
                  adj_label = "adjusted {label}",
                  estim_acro = NULL,
                  estim_label = NULL,
                  ci,
                  model_mv,
                  ref_sep = "",
                  ref_no = "",
                  estim_sep = ref_sep,
                  hide_n = TRUE,
                  vargrp_levels = "",
                  indent_type = "indent") {

  label_header <- if (!is.null(label_header)) glue("**{label_header}**") else ""
  label_stat <- if (!is.null(label_stat)) glue("**{label_stat}**") else ""
  
  if ("tbl_merge" %in% class(x)) {

    check_by <- x$tbls[[1]]$by
    
  } else check_by <- x$by

  if (!is.null(check_by)) {

    x <-
    .fmt_by(x,
            .check_by = check_by,
            .label_header = label_header,
            .label_overall = label_overall,
            .bold_p = bold_p)
    
    assign(".gtsum_output", 
           x$meta_data |> 
             select(variable, summary_type, stat_test_lbl, p.value) |> 
             mutate(p.value = style_pvalue(p.value, digits = 1, prepend_p = TRUE)), 
           envir = .GlobalEnv)

  } else {

    if (str_detect(class(x)[1], "tbl_(uv)?reg")) {

      x <-
      .fmt_reg(x,
               .adj_acro = adj_acro,
               .adj_label = adj_label,
               .estim_acro = estim_acro,
               .estim_label = estim_label,
               .ci = ci,
               .estim_sep = estim_sep,
               .model_mv = model_mv,
               .ref_sep = ref_sep,
               .ref_no = ref_no,
               .hide_n = hide_n,
               .label_header = label_header,
               .bold_p = bold_p)
      
      if ("tbl_regression" %in% class(x)) {
      
        assign(".gtsum_output",
               list(vars = x$table_body[c("variable", "var_type")],
                    model = x$model_obj |> tidy(exponentiate = TRUE)),
               envir = .GlobalEnv)
        
      } else {

        assign(".gtsum_output", x$meta_data, envir = .GlobalEnv)
        
      }
        
    } else {

      x <-
      .fmt_uni(x,
               .label_header = label_header,
               .label_stat = label_stat)

      assign(".gtsum_output",
             x$table_body[c("variable", "var_type")], 
             envir = .GlobalEnv)
      
    }

  }

  x <-
  .fmt_indent(x,
              .vargrp_levels = vargrp_levels,
              .indent_type = indent_type)

}
