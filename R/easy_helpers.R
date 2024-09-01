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
    .values <- map_dbl(values, ~ . - 1/10000)
    
    x <-
    x |> 
      mutate(!!name :=
               cut(x = {{ var }},
                   breaks = c(.min - 1, .values, .max + 1),
                   labels = labels,
                   right = FALSE),
             .after = all_of(var))
      
  } else {
    
    name <- glue("{var}_incr")
    
    x <-
    x |>
      mutate(!!name :=
               cut(x = {{ var }},
                   breaks = seq(...),
                   right = FALSE) |> 
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


#' Title
#'
#' @param dir 
#'
#' @return
#' @export
#'
#' @examples
#' 
easy_source <- \(dir = "scripts") {
  
  files <- list.files(dir)
  files <- files[!str_detect(files, "^_")]
  
  map(files, ~ source(glue("{dir}/{.}")))
  
}


#' Title
#'
#' @param df 
#' @param y 
#' @param x 
#' @param breaks 
#' @param color 
#' @param label_y 
#' @param label_x 
#'
#' @return
#' @export
#'
#' @examples
#' 
logit_lty <- \(df,
               y,
               x,
               breaks = 30,
               color = "#0099cc",
               label_y = "Logit(P {y})",
               label_x = x) {
  
  y <- enexpr(y)
  x <- enexpr(x)
  
  lst(data =
        df |> 
          mutate("{x}_cat" := cut(!!x, breaks = 40)) |> 
          summarise("mean_{x}" := mean(!!x),
                    "prop_{y}" := mean(as.numeric(!!y) == 2),
                    logit_prop = 
                      log(get(glue("prop_{y}")) / (1 - get(glue("prop_{y}")))),
                    .by = glue("{x}_cat")) |> 
          filter(!logit_prop %in% c(-Inf, Inf)),
      model =
        glm(data = df |> mutate("{x}_quantile" := cut_number(!!x, n = 4)),
            reformulate(glue("{x}_quantile"), y),
            family = binomial) |>
          tidy(exponentiate = TRUE,
               conf.int = TRUE) |> 
          mutate(p.value = style_pvalue(p.value, digits = 1)) |>
          select(term, estimate, starts_with("conf"), p.value),
      plot = 
        data |> 
          ggplot(aes(y = logit_prop,
                     x = get(glue("mean_{x}")))) +
          geom_point(alpha = 0.4) +
          geom_smooth(method = "lm",
                      formula = "y ~ x",
                      se = FALSE,
                      color = color) +
          labs(y = glue(label_y),
               x = glue(label_x)))
  
}

#' Title
#'
#' @param model 
#' @param limit_inf_num 
#' @param limit_sup_num 
#' @param limit_inf_color 
#' @param limit_sup_color 
#' @param obs_color 
#'
#' @return
#' @export
#'
#' @examples
#' 
cooksd <- \(model,
            limit_inf_num = 4,
            limit_sup_num = 25,
            limit_inf_color = "#0099cc",
            limit_sup_color = "red",
            obs_color = "black") {
    
  .out <-
  list(n = "{nrow(obs$inf)} total out. for {nrow(data)} total obs.",
       p = "({percent(nrow(obs$inf) / nrow(data), accuracy = .1)})")
  
  .list <-
  lst(data =
        model |>
          augment() |> 
          rownames_to_column("id"),
      limit =
        c(inf = limit_inf_num, 
          sup = limit_sup_num) |> 
          map_dbl(~ . / nrow(data)),
      outliers =
        limit |> 
          map(~ data |> 
                filter(.cooksd > .) |>
                pull(id)),
      obs =
        map(outliers, ~ data[., ]),
      plot =
        data |> 
          ggplot() +
          aes(x = as.numeric(id),
              y = .cooksd) +
          geom_jitter(color = obs_color,
                      alpha = 0.4) +
          geom_hline(yintercept = limit,
                     color = 
                       c(limit_inf_color,
                         limit_sup_color),
                     linewidth = 0.8) +
          annotate(geom = "label",
                   label = glue(.out$n, .out$p),
                   y = max(data$.cooksd),
                   x = 1,
                   size = 3,
                   hjust = 0,
                   vjust = 1) +
          xlab(NULL) +
          theme(axis.ticks.x = element_blank(),
                axis.text.x = element_blank()))
    
  .list[names(.list) != "outliers"]
  
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
flow_filter <- \(data, ...) {
  
  .exprs <- exprs(...)
  .exprs <- if (!is_named(.exprs)) set_names(.exprs) else .exprs
  
  .flow <-
  .exprs |>
    accumulate(~ filter(.x, !!.y), .init = data) |>
    map(~ glue("{nrow(.)} ({percent(nrow(.) / nrow(data), accuracy = .1)})"))
  
  assign("flow", .flow, envir = .GlobalEnv)
    
  data |> filter(!!!unname(.exprs))
  
}
