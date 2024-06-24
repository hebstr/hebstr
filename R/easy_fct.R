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
    list(cut = c(glue("<{cut}"), glue(">={cut}")),
         inf = glue("<{inf}"),
         btw_inf = glue("[{inf}-{.btw})"),
         btw = glue("[{inf}-{sup})"),
         btw_sup = glue("[{.btw}-{sup})"),
         sup = glue(">={sup}"))

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
