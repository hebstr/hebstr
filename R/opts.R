#' Title
#'
#' @param fr
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
opts_set <- \(fr = FALSE,
              ...) {

  dots <- list(...)

  .opts_set <-
  list(labs = list(sex_m = "Men",
                   sex_f = "Women",
                   yes = "Yes",
                   no = "No",
                   na = "NA"),
       sep = list(int = ": ",
                  ext = "; ",
                  conf = "; "),
       ci = list(lim = "[",
                 label = "95%CI",
                 cols = c("conf.low", "conf.high")),
       pvalue = list(format = ~ style_pvalue(., digits = 3),
                     seuil = 0.05),
       font = list(alpha = "luciole",
                   digit = "luciole"),
       palette = list(base = "#999999",
                      cold = c("#E1F6FF", "#0099CC"),
                      warm = c("#f5E3E0", "#BC3C29")))

  if (fr) {

    .opts_set <-
    list_modify(.opts_set,
                labs = list(sex_m = "Masculin",
                            sex_f = "Féminin",
                            yes = "Oui",
                            no = "Non"),
                sep = list(int = " : ",
                           ext = " ; ",
                           conf = " ; "),
                ci = list(label = "IC95%"))

  }

  switch(.opts_set$ci$lim,
         "[" = .lim <- c("[", "]"),
         "(" = .lim <- c("(", ")"))

  .opts_set$ci <-
  list(label = glue("{.lim[1]}{.opts_set$ci$label}{.lim[2]}"),
       data = glue("{.lim[1]}{{{.opts_set$ci$cols[1]}}}{.opts_set$sep$conf}{{{.opts_set$ci$cols[2]}}}{.lim[2]}"))

  .opts_set <- list_modify(.opts_set, !!!dots)

  check_font(.opts_set$font)

  return(.opts_set)

}


#' Title
#'
#' @param ...
#' @param sep
#'
#' @return
#' @export
#'
#' @examples
#'
opts_acro <- \(...,
               sep) {

  e <- env("~" = \(x, y) glue("{enexpr(x)}{sep}{y}"))

  .opts_acro <-
  list(...) |>
    map(~ eval(enexpr(.), e)) %>%
    set_names(str_extract(., "\\w+"))

  return(.opts_acro)

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
opts_fig <- \(...) {

  dots <- list(...)

  .opts_fig <-
  list(theme = list(stat = "count",
                    size = 2.5))

  .opts_fig <- list_modify(.opts_fig, !!!dots)

  return(.opts_fig)

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
opts_finalize <- \(...) {

  .opts_set <- append(opts, list(...))

  ls_env <- ls(envir = .GlobalEnv)

  rm(list = ls_env[str_starts(ls_env, "opts_")],
     envir = .GlobalEnv)

  assign("opts", .opts_set, envir = .GlobalEnv)

}
