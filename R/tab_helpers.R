#' Title
#'
#' @param data 
#' @param variable 
#' @param by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
quanti.test.para <- \(data,
                      variable,
                      by,
                      ...) {
  
  .var <- data[[variable]]
  .by <- data[[by]]
  
  if (nlevels(factor(.by)) == 2) {
      
    tidy(t.test(.var ~ .by, var.equal = TRUE))
      
  } else {
    
    tidy(anova(.var ~ .by, var.equal = TRUE))
      
  }
  
}


#' Title
#'
#' @param data 
#' @param variable 
#' @param by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
quanti.test.nonpara <- \(data, 
                         variable, 
                         by,
                         ...) {
  
  .var <- data[[variable]]
  .by <- data[[by]]
  
  if (nlevels(factor(.by)) == 2) {
      
    tidy(wilcox.test(.var ~ .by, exact = FALSE, correct = FALSE))
      
  } else {
    
    tidy(kruskal.test(.var ~ .by))
      
  }
  
}


#' Title
#'
#' @param data 
#' @param variable 
#' @param by 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
quali.test <- \(data, variable, by, ...) {
  
  .var <- data[[variable]]
  .by <- factor(data[[by]])
  
  chisq_is_correct <- \(correct) {
  
    chisq.test(.var, .by, correct = correct) |>
    suppressWarnings()
    
  }
  
  chisq.test.no.correct <- chisq_is_correct(FALSE)
  
  is_under <- \(n) {
    
    chisq.test.no.correct$expected |>
      data.frame() |>
      filter(if_any(everything(), ~ . < n)) |>
      nrow() > 0
    
  }

  if (is_under(5)) {
    
    if (!is_under(2)) {
  
      tidy(chisq_is_correct(TRUE))
  
    } else {
      
      tidy(fisher.test(.var, .by))
      
    }
    
  } else {
    
    tidy(chisq.test.no.correct)
    
  }
  
}


#' Title
#'
#' @param data
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
all_dichotomous_uv <- \(data, ...) {

  dots <- c(...) %||% names(data)
  
  level <- map_int(dots, ~ nlevels(data[[.]]))

  dots[level == 2]

}


#' Title
#'
#' @param data
#' @param estim_col
#' @param ci_col
#' @param name 
#' @param ci_data
#' @param digit 
#' @param percent
#' @param keep 
#'
#' @return
#' @export
#'
#' @examples
#'
merge_estim_ci <- \(data,
                    estim_col = "estimate",
                    ci_col = starts_with("conf."),
                    name = "estimate_ci",
                    ci_data = check_opts(ci$data),
                    digit = 2,
                    percent = FALSE,
                    keep = FALSE) {

  name <- glue(name)
  
  vars <- expr(c(all_of(estim_col), all_of(ci_col)))
  
  if (percent) multi <- 100 else multi <- 1

  data <-
  data |>
    mutate(across(!!vars,
                  ~ round(. * multi, 2) |>
                    format(nsmall = 2)),
           "{name}" := glue("{get(estim_col)} ", ci_data))
  
  if (!keep) data <- data |> select(-eval(vars))
  
  return(data)

}


#' Title
#'
#' @param data
#' @param var
#' @param new_lab
#' @param ref_lab 
#' @param ref_level 
#' @param tolower_level 
#'
#' @return
#' @export
#'
#' @examples
#'
easy_relab <- \(data,
                var,
                new_lab = "{var_label}",
                ref_lab = " — ref",
                ref_level = data$table_body$reference_level,
                tolower_level = TRUE) {
  
  ref_sep <-
  data$table_body$label |>
    str_extract(glue("(?<={ref_lab}).\\s*")) |>
    na.omit() |>
    unique()

  if (tolower_level) ref_level <- tolower(ref_level) else ref_level
  
  str <- "{glue(new_lab)}{ref_lab}{ref_sep}{ref_level}"

  data |>
    modify_table_body(
      ~ . |>
        mutate(label = ifelse(variable %in% var, glue(str), label))
    )

}


#' Title
#'
#' @param data 
#' @param vars 
#' @param levels 
#' @param rows 
#' @param pvalue_mv 
#' @param note 
#'
#' @return
#' @export
#'
#' @examples
#' 
add_note <- \(data,
              vars = NULL,
              levels = NULL,
              rows = NULL,
              pvalue_mv = NULL,
              note) {
  
  if (is.null(pvalue_mv)) {
  
    rows <- enexpr(rows)
    
    if (!is.null(vars)) {
      
      vars <- expr(variable %in% !!vars & row_type == "label")
      
    } else if (!is.null(levels)) {
      
      vars <- expr(label %in% !!levels)
    
    } else if (!is.null(rows)) {
      
      vars <- enexpr(rows)
    
    }

    tab_footnote(data = data,
                 footnote = note,
                 locations = cells_body(columns = label, rows = !!vars))
    
  } else {
    
    tab_footnote(data = data,
                 footnote = note,
                 locations = cells_column_labels(glue("p.value_{pvalue_mv + 1}")))
    
  }

}


#' Title
#'
#' @param x 
#' @param cap 
#'
#' @return
#' @export
#'
#' @examples
#' 
fct_str <- \(x, 
             sep, 
             cap = TRUE) {
  
  str <-
  x |>
    str_squish() |> 
    fct_count(sort = TRUE) |> 
    drop_na() |> 
    mutate(str = glue("{f} [{n}]")) |> 
    pull(str) |> 
    str_flatten(sep) |> 
    glue(".")
    
  if (cap) str <- str |> tolower() %>% str_cap(toupper, .)

  return(str)
    
}


#' Title
#'
#' @param fct 
#' @param chr 
#' @param min 
#'
#' @return
#' @export
#'
#' @examples
#' 
fct_other_str <- \(fct, chr, min) {
  
  fct <-
  fct |> 
    fct_count(sort = TRUE) |> 
    filter(f != "Other", n < min) |> 
    mutate(str = glue("{f} ({n})")) |> 
    pull(str) |> 
    str_flatten_comma() |> 
    str_to_lower()
  
  fct <- str_cap(toupper, fct)
  
  chr <- fct_str(chr, cap = FALSE)
  
  paste(fct, chr, sep = ", ")
   
}

#' Title
#'
#' @param data 
#' @param var 
#' @param min 
#' @param sep 
#'
#' @return
#' @export
#'
#' @examples
#' 
fct_keep <- \(data,
              var,
              min,
              sep) {

  x <-
  data |> 
    separate_longer_delim(!!var, delim = regex("\\s*(,|et)\\s*")) |> 
    count(!!var := get(var), sort = TRUE) |>
    drop_na() |> 
    split(~ n >= min) |> 
    set_names(c("drop", "keep"))
  
  y <-
  list(keep =
         x$keep |> 
           pull(!!var) |> 
           as.character(),
       drop = 
         x$drop |> 
           mutate(str = glue("{get(var)} [{n}]")) |> 
           pull(str) |> 
           str_flatten(sep) %>%
           glue("."))

  return(y)
  
}


#' Title
#'
#' @param x 
#' @param name 
#' @param levels 
#' @param label_indent 
#' @param levels_indent 
#' @param .before 
#' @param .after 
#'
#' @return
#' @export
#'
#' @examples
#' 
add_label <- \(x,
               name,
               levels,
               indent = 0) {
  
  .before_index <- grep(levels[1], x$table_body$variable)[1]
  
  x <-
  x |> 
    modify_table_body(
      ~ . |>
        add_row(label = name, 
                .before = .before_index)
    ) |>
    modify_column_indent(columns = label,
                         rows = label == name,
                         indent = indent) |> 
    modify_column_indent(columns = label,
                         rows = variable %in% levels,
                         indent = indent + 4)

}


#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
#' 
str_na_mv <- \(data) {
  
  n_total <- nrow(data)
  
  na <-
  lst(n = data |> filter(if_any(everything(), is.na)) |> nrow(),
      p = label_p()(n / n_total),
      obs =
        case_when(n == 0 ~ "aucune observation",
                  n == 1 ~ glue("{n} observation"),
                  .default = glue("{n} observations ({p})")))
  
  glue("{n_total} observations, {na$obs} contenant a minima une données manquante")

}


#' Title
#'
#' @param data 
#' @param exclude 
#'
#' @return
#' @export
#'
#' @examples
#' 
show_single_row <- \(data,
                     exclude = names(data[, 1])) {
  
  all_dichotomous <- expr(c(where(~ nlevels(.) == 2), -all_of(exclude)))

  data |> 
    mutate(across(!!all_dichotomous, ~ if_else(as.numeric(.) == 1, 0, 1)))
    
}

