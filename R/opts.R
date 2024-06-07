check_font <- \(...) {

  font <- str_cap(toupper, c(...))

  is_installed <- font %in% systemfonts::system_fonts()$family

  if (FALSE %in% is_installed) {

    which_font <-
    data.frame(font, is_installed) |>
      filter(!is_installed) |>
      pull(font) |>
      unique()

    cli::cli_abort("{which_font} {?is/are} not installed")

  }

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
opts_set <- \(...) {

  dots <- list(...)

  .set <-
  list(labs = list(sex_m = "Masculin",
                   sex_f = "Féminin",
                   yes = "Oui",
                   no = "Non",
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
