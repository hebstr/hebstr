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
  col_replace <- glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
  map(c(...),
      ~ list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
        unlist())

  replace_list <- list2("(\n*{replace})+\n*" := col_replace)

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

  dots <- list(...)

  list(name =
         map(dots, ~ unname(.[1])) |>
           list_flatten() |>
           unlist(),
       label =
         map(dots, ~ unname(.[2])) |>
           list_flatten() |>
           unlist(),
       levels =
         map(dots, ~ unname(.[3])) |>
           list_flatten())

}


#' Title
#'
#' @param x 
#' @param var 
#' @param incr 
#' @param drop 
#' @param values 
#' @param labels 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
easy_cut <- \(x, 
              var, 
              incr = FALSE,
              drop = FALSE,
              values = NULL, 
              labels = NULL,
              ...) {
  
  var <- enexpr(var)
  
  if (!incr) {
    
    name <- glue("{var}_cat")
    
    .min <- min(x[[var]], na.rm = TRUE)
    .max <- max(x[[var]], na.rm = TRUE)
    
    x <-
    x |> 
      mutate(!!name :=
               cut(x = {{ var }},
                   breaks = c(.min - 1, values, .max + 1),
                   labels = labels),
             .after = all_of(var))
      
  } else {
    
    name <- glue("{var}_incr")
    
    x <-
    x |>
      mutate(!!name :=
               cut(x = {{ var }}, breaks = seq(...), include.lowest = FALSE) |> 
               as.numeric(),
             .after = all_of(var))
    
  }
  
  if (drop) {
    
    x <-
    x |> 
      select(-!!var) |> 
      rename(!!var := all_of(name))
    
  }
  
  return(x)
  
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

lst(coord =
      x |>
        broom::tidy("rotation") |>
        tidyr::pivot_wider(names_from = "PC",
                           names_prefix = "PC",
                           values_from = "value") |>
        mutate(column = str_remove_all(column, "hamd"),
               .keep = "all"),
    contrib =
      coord |>
        mutate(across(matches("PC"), ~ . ^ 2 / sum(. ^ 2))),
    weight =
      coord["column"] |>
        mutate(PC1 = contrib$PC1 / max(contrib$PC1)) |>
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
    mutate(model = map(splits, .f))

  boot <-
  list(estim_data = tidy,
       fitted = augment) |>
    map(~ boot |>
          mutate(coef = map(model, .)))

  boot <-
  list(estimate =
         list(data = boot$estim_data,
              int = boot$estim_data |> int_pctl(coef)),
       fitted = boot$fitted)

  assign("boot", boot, envir = .GlobalEnv)

  print(boot$estim$int)

}


#' Title
#'
#' @param data
#' @param model
#' @param y
#' @param vars
#' @param pv
#'
#' @return
#' @export
#'
#' @examples
#'
p_picking <- \(data,
               model,
               y,
               vars,
               pv) {

  fit <- expr(list(reformulate(., y), data = data))

  vars[!vars %in% y] |>
    map(~ do.call(model, eval(fit)) |>
          tidy() |>
          mutate(variable = str_extract(term, str_u(vars)))) |>
    list_rbind() |>
    filter(variable %in% vars,
           p.value <= pv) |>
    pull(variable)

}


#' Title
#'
#' @param x
#' @param column
#' @param digits
#' @param seuil
#' @param table
#'
#' @return
#' @export
#'
#' @examples
#'
p_shortenr <- \(x,
                column = p.value,
                digits = 3,
                seuil = 0.001,
                table = TRUE) {

  column <- enexpr(column)

  if (table) inf <- "<" else inf <- "< "
  if (table) sup <- "" else sup <- "= "

  x |>
    rowwise() |>
    mutate(!!column :=
             if_else(!!column < seuil,
                     seuil,
                     round(!!column, digits)),
           !!column :=
             if_else(!!column == seuil,
                     glue(inf, !!column),
                     glue(sup, !!column)))

}


#' Title
#'
#' @param data 
#' @param .var
#' @param .min
#' @param .fun 
#'
#' @return
#' @export
#'
#' @examples
#'
pct_min <- \(data,
             .var,
             .min,
             .fun = "max") {

  .count <-
  data |>
    count("{.var}" := get(.var)) |>
    mutate(p = n / do.call(.fun, list(n))) |>
    filter(p >= .min) |> 
    pull(.var)

  data |>
    filter(get(.var) %in% .count)

}


#' Title
#'
#' @param file
#' @param dir
#'
#' @return
#' @export
#'
#' @examples
#'
read_png <- \(file, dir = "output") {

  glue("{dir}/{file}.png") |>
    readPNG() |>
    rasterGrob()

}
