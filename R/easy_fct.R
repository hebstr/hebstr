#' Recode or bin a variable in a data frame
#'
#' Dual-purpose helper that either recodes a categorical variable
#' using [forcats::fct_recode()] or bins a numeric variable into
#' ordered factor levels. The dispatch is automatic based on the
#' column type.
#'
#' @section Categorical variables:
#' When `var` is non-numeric, `...` and `.dots` are forwarded to
#' [forcats::fct_recode()] as `"new_level" = "old_level"` pairs.
#' The resulting factor is releveled in the order of the supplied
#' mappings.
#'
#' @section Numeric variables:
#' Three binning modes are available:
#' - **`inf` + `sup`**: 3 classes: `<inf`, `[inf-sup)`, `>=sup`.
#' - **`inf` + `sup` + `.btw`**: 4 classes with an intermediate
#'   breakpoint at `.btw`.
#' - **`cut`**: 2 classes via [base::cut()]: `<cut`, `>=cut`.
#'
#' @param x A data frame.
#' @param var <[`data-masking`][rlang::args_data_masking]> Column to
#'   recode or bin (unquoted).
#' @param inf,sup Lower and upper bounds for numeric binning.
#'   Both required when `cut` is `NULL`.
#' @param cut Single numeric threshold for a binary split. When
#'   supplied, `inf` and `sup` are ignored.
#' @param .btw Optional intermediate breakpoint between `inf` and
#'   `sup`, producing 4 classes instead of 3.
#' @param .name Name for the output column (defaults to `var`).
#'   Useful to create a new column instead of overwriting.
#' @param .dots A named list of recode mappings, as an alternative
#'   to passing them via `...`.
#' @param ... For categorical variables: recode mappings forwarded
#'   to [forcats::fct_recode()]. For numeric `cut` mode: extra
#'   arguments forwarded to [base::cut()].
#'
#' @return A data frame with the recoded or binned column.
#' @export
#'
#' @examples
#' df <- data.frame(age = c(20, 45, 60, 75))
#' easy_fct(df, age, inf = 30, sup = 65)
#'
easy_fct <- \(
  x,
  var,
  inf = NULL,
  sup = NULL,
  cut = NULL,
  .btw = NULL,
  .name = enexpr(var),
  .dots = list(),
  ...
) {
  var <- enexpr(var)

  if (!is.numeric(x[[var]])) {
    dots <- c(exprs(...), .dots)

    x |>
      mutate(
        !!var := inject(fct_recode(!!var, !!!dots)) |>
          fct_relevel(names(dots))
      )
  } else {
    lv <-
      list(
        cut = c(glue("<{cut}"), glue(">={cut}")),
        inf = glue("<{inf}"),
        btw_inf = glue("[{inf}-{.btw})"),
        btw = glue("[{inf}-{sup})"),
        btw_sup = glue("[{.btw}-{sup})"),
        sup = glue(">={sup}")
      )

    if (is.null(cut)) {
      if (is.null(inf) || is.null(sup)) {
        cli_abort(
          "{.arg inf} and {.arg sup} are required when {.arg cut} is NULL."
        )
      }

      if (is.null(.btw)) {
        cond <-
          exprs(
            !!var < inf ~ lv$inf,
            !!var >= inf & !!var < sup ~ lv$btw,
            !!var >= sup ~ lv$sup
          )

        lvs <- c(lv$inf, lv$btw, lv$sup)
      } else {
        cond <-
          exprs(
            !!var < inf ~ lv$inf,
            !!var >= inf & !!var < .btw ~ lv$btw_inf,
            !!var >= .btw & !!var < sup ~ lv$btw_sup,
            !!var >= sup ~ lv$sup
          )

        lvs <- c(lv$inf, lv$btw_inf, lv$btw_sup, lv$sup)
      }

      x |>
        mutate(!!.name := inject(case_when(!!!cond)) |> fct_relevel(lvs))
    } else {
      x |>
        mutate(
          !!.name := cut(
            !!var,
            breaks = c(-Inf, cut, Inf),
            labels = lv$cut,
            right = FALSE,
            ...
          )
        )
    }
  }
}
