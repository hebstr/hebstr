#' Title
#'
#' @param ...
#' @param .sep 
#' @param .auto 
#' @param .tolower 
#'
#' @return
#' @export
#'
#' @examples
#'
acro <- \(...,
          .sep = ":",
          .auto = TRUE,
          .tolower = FALSE) {
  
  e <- if (!.tolower) {
    
    env("~" = \(x, y) glue("{enexpr(x)}{sep}{y}"))
    
  } else {
    
    env("~" = \(x, y) glue("{tolower(enexpr(x))}{sep}{y}"))
    
  }

  .fun <- \(...) {
  
    list(...) |>
      map(~ eval(enexpr(.), e)) %>%
      set_names(str_extract(., glue(".+(?={sep})")))
  
  }

  if (getOption("OutDec") == ".") {
  
  sep <- glue("{.sep} ")
    
  base <-
  .fun(SD ~ "standard deviation",
       IQR ~ "interquartile range",
       Q1 ~ "1st quartile",
       Q3 ~ "3rd quartile",
       `95%CI` ~ "95% confidence interval")
  
  } else {
    
    sep <- glue(" {.sep} ")
    
    base <-
    .fun(SD ~ "écart-type",
         IQR ~ "intervalle interquartile",
         Q1 ~ "1er quartile",
         Q3 ~ "3e quartile",
         `IC95%` ~ "intervalle de confiance à 95%")
    
  }
  
  .acro <- if (.auto) list_modify(base, !!!.fun(...)) else .fun(...)
  
  return(.acro)

}


#' Title
#'
#' @param x
#' @param acro_list
#'
#' @return
#' @export
#'
#' @examples
#'
acro_extract <- \(x,
                  acro_list) {

  .str <-
  acro_list |> 
    names() |>
    str_c(collapse = "\\b|\\b")

  x |>
    str_extract(glue("\\b{.str}\\b")) |>
    na.omit() |>
    unique()

}


#' Title
#'
#' @param ... 
#' @param collapse
#'
#' @return
#' @export
#'
#' @examples
#'
acro_str <- \(..., 
              collapse = NULL) {

  acro <- str_c(c(...), collapse = collapse)
  
  if (acro != "") glue("{acro}.") else NULL

}


#' Title
#'
#' @param x
#' @param vars
#' @param acro_list
#' @param acro_sep
#'
#' @return
#' @export
#'
#' @examples
#'
acro_match <- \(x,
                vars = names(x),
                acro_list,
                acro_sep) {

.acro <-
vars |>
  map(~ with(x, get(.)) |>
        label_attribute()) |>
  unlist() |>
  acro_extract(acro_list)

acro_str(with(acro_list, mget(.acro)),
         collapse = acro_sep)

}
