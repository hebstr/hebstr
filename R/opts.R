#' @param suivi_an
#'
#' @param fdr
#' @param labs
#' @param sep
#' @param sep_space
#' @param ci
#' @param pvalue
#' @param font
#' @param palette
#'
#' @export
#'
opts_set <- \(suivi_an = NULL,
              fdr = NULL,
              labs = NULL,
              sep,
              sep_space = FALSE,
              ci, pvalue,
              font, palette) {

if (!is.null(suivi_an)) {

  if (!suivi_an %in% 1:5) {

  abort("suivi_an doit être un entier compris entre 1 et 5")

  }

  suivi_jr <- suivi_an * 365

} else suivi_jr <- NULL

sep <- purrr::map(sep, ~ paste(., ""))

if (sep_space) sep <- purrr::map(sep, ~ paste("", .))

switch(ci$lim,
       "[" = { ci_low <- "[" ; ci_high <- "]" },
       "(" = { ci_low <- "(" ; ci_high <- ")" })

ci <-
list(label = paste0(ci_low, ci$label, ci_high),
     data = paste0(ci_low, "{conf.low}", sep$.ci, " {conf.high}", ci_high))

if (rlang::is_false(fdr$include)) fdr$output_suffix <- ""

assign("opts",
       list(set = c(suivi_an = suivi_an,
                    suivi_jr = suivi_jr,
                    fdr = list(fdr),
                    pvalue = list(pvalue),
                    labs = list(labs),
                    sep = list(sep),
                    ci = list(ci),
                    font = list(font),
                    palette = list(palette))),
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

  opts <- append(opts, list(...))

  ls_env <- ls(envir = .GlobalEnv)

  rm(list = ls_env[str_starts(ls_env, "opts_")],
     envir = .GlobalEnv)

  return(opts)

}
