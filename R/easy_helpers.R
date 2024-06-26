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
               mutate(across(dplyr::matches("PC"),
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
#' @param x
#' @param .var
#' @param .min
#' @param denom
#'
#' @return
#' @export
#'
#' @examples
#'
pct_min <- \(x,
             .var,
             .min,
             denom = "sum") {

  .count <-
  x |>
    count("{.var}" := get(.var)) |>
    mutate(p = n / do.call(denom, list(n))) |>
    filter(p > .min)

  x[.var] |>
    filter(get(.var) %in% .count[[.var]])

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
