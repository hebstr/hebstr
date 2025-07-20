#' Title
#'
#' @param x arg
#' @param var arg
#' @param inf arg
#' @param sup arg
#' @param cut arg
#' @param .btw arg
#' @param .name arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_fct <- \(x,
              var,
              inf = NULL,
              sup = NULL,
              cut = NULL,
              .btw = NULL,
              .name = enexpr(var),
              ...) {

  var <- enexpr(var)

  if (!is.numeric(x[[var]])) {

    dots <- exprs(...)

    x |>
      mutate(!!var :=
               inject(fct_recode(!!var, !!!dots)) |>
                      fct_relevel(names(dots)))

  } else {

    lv <-
    list(cut = c(glue("<{cut}"), glue(">={cut}")),
         inf = glue("<{inf}"),
         btw_inf = glue("[{inf}-{.btw})"),
         btw = glue("[{inf}-{sup})"),
         btw_sup = glue("[{.btw}-{sup})"),
         sup = glue(">={sup}"))

    if (is.null(cut)) {

      if (is.null(.btw)) {

        cond <-
        exprs(!!var < inf ~ lv$inf,
              !!var >= inf & !!var < sup ~ lv$btw,
              !!var >= sup ~ lv$sup)

        lvs <- c(lv$inf, lv$btw, lv$sup)

      } else {

        cond <-
        exprs(!!var < inf ~ lv$inf,
              !!var >= inf & !!var < .btw ~ lv$btw_inf,
              !!var >= .btw & !!var < sup ~ lv$btw_sup,
              !!var >= sup ~ lv$sup)

        lvs <- c(lv$inf, lv$btw_inf, lv$btw_sup, lv$sup)

      }

      x |> 
        mutate(!!.name :=
                 inject(case_when(!!!cond)) |> fct_relevel(lvs))

    } else {

      x |>
        mutate(!!.name :=
                 cut(!!var,
                     breaks = c(0, cut, Inf),
                     labels = lv$cut,
                     right = FALSE,
                     ...))

    }

  }

}
