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
#' @param merge_col
#' @param ci_data
#' @param estim_ci_digit
#' @param pvalue_digit
#' @param percent
#'
#' @return
#' @export
#'
#' @examples
#'
merge_estim_ci <- \(data,
                    estim_col = "estimate",
                    ci_col = starts_with("conf."),
                    merge_col = "estimate_ci",
                    ci_data,
                    estim_ci_digit = 2,
                    percent = FALSE) {

  if (percent) multi <- 100 else multi <- 1

  .rnd <- \(x, n) {

    x |>
    round(n) |>
      format(nsmall = n)

  }

  data |>
    mutate(across(c(all_of(estim_col), all_of(ci_col)),
                  ~ .rnd(. * multi, estim_ci_digit)),
           "{merge_col}" := glue("{get(estim_col)} ", ci_data))

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
#' @param note 
#'
#' @return
#' @export
#'
#' @examples
#' 
gt_note <- \(data,
             vars = NULL,
             levels = NULL,
             rows = NULL,
             note) {
  
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

}


#' Title
#'
#' @param data 
#' @param var 
#' @param sup_to 
#'
#' @return
#' @export
#'
#' @examples
#' 
fct_keep <- \(data,
              var,
              sup_to) {

  x <-
  data |> 
    count(!!var := get(var), sort = TRUE) |>
    drop_na() |> 
    split(~ n > sup_to) |> 
    set_names(c("drop", "keep"))
  
  y <-
  list(keep = pull(x$keep, !!var),
       drop = 
         x$drop |> 
           mutate(str = glue("{get(var)} ({n})")) |> 
           pull(str) |> 
           str_flatten_comma() %>%
           glue("."))

  return(y)
  
}
