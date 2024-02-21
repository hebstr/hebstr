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
