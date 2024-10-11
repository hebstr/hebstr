#' Title
#'
#' @param reset 
#'
#' @return
#' @export
#'
#' @examples
#' 
lang_fr <- \(reset = FALSE) {
  
  if (reset) {
    
    options(OutDec = ".")
  
    reset_gtsummary_theme()
    
    cli_alert_info("Setting language: EN")
    
  } else {
  
    options(OutDec = ",")
    
    theme_gtsummary_language(language = "fr",
                             big.mark = " ") |> 
      suppressMessages()
    
    cli_alert_info("Setting language: FR")
    
  }
  
}

#' Title
#'
#' @param check_fonts 
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
opts_set <- \(check_fonts = TRUE,
              ...) {

  dots <- lst(...)

  .opts_set <-
  lst(auto = \(x) easy_tbl(x),
      labs =
        list(header = "Characteristic",
             overall = "Overall",
             spanner = glue("{c('Univariable', 'Multivariable')} analysis")),
      sep = 
        list(int = ": ",
             ext = "; ",
             conf = "; "),
      ci =
        list(lim = "[",
             label = "95%CI",
             cols = c("conf.low", "conf.high")),
      digits =
        list(all_continuous() ~ 1,
             all_categorical() ~ c(0, label_percent(accuracy = .1, suffix = ""))),
      pvalue = 
        list(format = label_style_pvalue(digits = 2),
             seuil = 0.05),
      font =
        list(alpha = "luciole",
             digit = "luciole"),
      palette = 
        list(base = "#999999",
             cold = c("#E1F6FF", "#0099EE"),
             warm = c("#f5E3E0", "#BC3C33")))

  if (getOption("OutDec") == ",") {

    .opts_set <-
    .opts_set |> 
      list_modify(labs = 
                    list(header = "Variable",
                         overall = "Total",
                         spanner = glue("Analyse {c('univariable', 'multivariable')}")),
                  sep =
                    list(int = " : ",
                         ext = " ; ",
                         conf = " ; "),
                  ci =
                    list(label = "IC95%"))

  }

  switch(.opts_set$ci$lim,
         "[" = .lim <- c("[", "]"),
         "(" = .lim <- c("(", ")"))

  .opts_set$ci <-
  list(label = glue("{.lim[1]}{.opts_set$ci$label}{.lim[2]}"),
       data = glue("{.lim[1]}{{{.opts_set$ci$cols[1]}}}{.opts_set$sep$conf}{{{.opts_set$ci$cols[2]}}}{.lim[2]}"))

  .opts_set <- list_modify(.opts_set, !!!dots)

  if (check_fonts) check_fonts(.opts_set$font)

  return(.opts_set)

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
