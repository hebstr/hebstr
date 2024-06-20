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

  .set <-
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

    .set <-
    purrr::list_modify(.set,
                       labs = list(sex_m = "Masculin",
                                   sex_f = "Féminin",
                                   yes = "Oui",
                                   no = "Non"),
                       sep = list(int = " : ",
                                  ext = " ; ",
                                  conf = " ; "),
                       ci = list(label = "IC95%"))

  }

  switch(.set$ci$lim,
         "[" = .lim <- c("[", "]"),
         "(" = .lim <- c("(", ")"))

  .set$ci <-
  list(label = glue::glue("{.lim[1]}{.set$ci$label}{.lim[2]}"),
       data = glue::glue("{.lim[1]}{{{.set$ci$cols[1]}}}{.set$sep$conf}{{{.set$ci$cols[2]}}}{.lim[2]}"))

  .set <- purrr::list_modify(.set, !!!dots)

  check_font(.set$font)

  assign("opts",
         list(set = .set),
         envir = .GlobalEnv)

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

  .opts <- append(opts, list(...))

  ls_env <- ls(envir = .GlobalEnv)

  rm(list = ls_env[stringr::str_starts(ls_env, "opts_")],
     envir = .GlobalEnv)

  assign("opts", .opts, envir = .GlobalEnv)

}
