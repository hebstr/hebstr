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
#' @param .default_font 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' 
set_opts <- \(.default_font = "trebuchet ms",
              ...) {
  
  dots <- lst(...)

  .fonts <- \(x) check_fonts(.default = .default_font, .auto = x)
  
  .label <-
  list(n = 
         label_number(accuracy = .1, 
                      decimal.mark = getOption("OutDec")),
       p = 
         label_percent(accuracy = .1,
                       suffix = "",
                       decimal.mark = getOption("OutDec")))
  
  .opts_set <-
  lst(parametric = nullfile(),
      qt_stat =
        list(min = c("Min" = "{min}"),
             q1 = c("Q1" = "{p25}"),
             median = c("Median (IQR)" = "{median} ({p25}—{p75})"),
             q3 = c("Q3" = "{p75}"),
             max = c("Max" = "{max}"),
             mean = c("Mean±SD" = "{mean}±{sd}")),
        ql_stat = 
          list(n = c("n (%)" = "{n} ({p})")),
      labs =
        list(sex = list(m = "Male", f = "Female"),
             bin = list(no = "No", yes = "Yes"),
             missing = "Missing data",
             header = "Characteristic",
             overall = "Overall",
             spanner = glue("{c('Univariable', 'Multivariable')} analysis")),
      sep = 
        list(int = ": ",
             ext = "; "),
      ci =
        list(lim = "[",
             sep = "; ",
             label = "95%CI",
             data = c("conf.low", "conf.high")),
      digits =
        list(all_continuous() ~ c(1, .label$n, .label$n),
             all_categorical() ~ c(0, .label$p)),
      pvalue = 
        list(format = label_style_pvalue(digits = 2),
             seuil = 0.05),
      font =
        list(alpha = "luciole",
             digit = "luciole"),
      color = 
        list(base = "#999",
             cold = c("#E0F9FF", "#0099EE"),
             warm = c("#FFEAEA", "#CC0C00")),
      palette = 
        c(color$base, 
          color$cold[2]))

  if (getOption("OutDec") == ",") {

    .opts_set <-
    .opts_set |> 
      list_modify(qt_stat =
                    list(median = c("Médiane (IQR)" = "{median} ({p25}—{p75})"),
                         mean = c("Moyenne±SD" = "{mean}±{sd}")),
                  labs = 
                    list(sex = list(m = "Hommes", f = "Femmes"),
                         bin = list(no = "Non", yes = "Oui"),
                         missing = "Donnée manquante",
                         header = "Variable",
                         overall = "Total",
                         spanner = glue("Analyse {c('univariable', 'multivariable')}")),
                  sep =
                    list(int = " : ",
                         ext = " ; "),
                  ci =
                    list(sep = " ; ",
                         label = "IC95%"))

  }

  .vars <- \(...) {
  
    data <- easy_descr(...)
  
    cap <- \(x) str_cap(tolower, names(x))
    
    list(test =
           list(data$qt$vars$parametric ~ "quanti.test.para",
                data$qt$vars$nonparametric ~ "quanti.test.nonpara",
                all_categorical() ~ "quali.test"),
         stat =
           list(data$qt$vars$parametric ~ data$qt$stat$mean,
                data$qt$vars$nonparametric ~ data$qt$stat$median,
                all_categorical() ~ data$ql$stat$n),
         label =
           list(data$qt$vars$parametric ~ cap(data$qt$stat$mean),
                data$qt$vars$nonparametric ~ cap(data$qt$stat$median),
                all_categorical() ~ cap(data$ql$stat$n)))
    
  }
  
  .ci <- \(lim, sep, label, data) {
  
    lim <- if (lim == "[") c("[", "]") else c("(", ")")
    
    lst(label = glue("{lim[1]}{label}{lim[2]}"),
        data = glue("{lim[1]}{{{data[1]}}}{sep}{{{data[2]}}}{lim[2]}"))
    
  }

  assign("opts", list_modify(.opts_set, !!!dots), envir = .GlobalEnv)
  
  opts <-
  opts |>
    list_modify(vars = \(x) .vars(x, !!!with(opts, list(parametric, qt_stat, ql_stat))),
                qt_stat_wide =
                  opts$qt_stat |> 
                    list_modify(median =
                                  list2("{str_remove(names(opts$qt_stat$median), '\\\\s.+')}" :=
                                          str_remove(opts$qt_stat$median, "\\s.+")) |> 
                                    unlist()) |>
                    list_c(),
                ci = .ci(!!!opts$ci),
                font = 
                  list(alpha = .fonts(opts$font[[1]]),
                       digit = .fonts(opts$font[[2]])),
                gt = 
                  lst(acro_list = opts$acro,
                      acro_sep = opts$sep$ext,
                      alpha = .fonts(opts$font[[1]]),
                      digit = .fonts(opts$font[[2]]),
                      color = opts$color$cold[1],
                      docx = if (exists("docx")) docx else FALSE)) |> 
    inject()

  if (identical(opts$font[[1]], opts$font[[2]])) opts$font <- opts$font[[1]]
  
  assign("opts", opts, envir = .GlobalEnv)
  
  return(opts)

}

#' Title
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
#' 
check_opts <- \(data) {
  
  if (exists("opts")) { 
    
    data <- with(opts, eval(enexpr(data)))
    
    return(data)
    
  }
  
  else {
    
    cli_abort("opts not found")
    
  }

}
