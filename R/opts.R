#' Title
#'
#' @param labs
#' @param sep
#' @param ci
#' @param pvalue
#' @param font
#' @param palette
#'
#' @return
#' @export
#'
#' @examples
#'
opts_set <- \(labs = NULL,
              sep = NULL,
              ci = NULL,
              pvalue = NULL,
              font = NULL,
              palette = NULL) {

  .labs <-
  list(sex_m = "Masculin", sex_f = "Féminin",
       yes = "Oui", no = "Non", na = "NA") |>
    list_modify(!!!labs)

  .sep <-
  list(int = ": ", ext = "; ", conf = "; ") |>
    list_modify(!!!sep)

  .ci <-
  list(lim = "[", label = "95%CI") |>
    list_modify(!!!ci)

  switch(.ci$lim,
         "[" = .lim <- c("[", "]"),
         "(" = .lim <- c("(", ")"))

  .ci <-
  list(label = glue("{.lim[1]}{.ci$label}{.lim[2]}"),
       data = glue("{.lim[1]}{{conf.low}}{.sep$conf}{{conf.high}}{.lim[2]}"))

  .pvalue <-
  lst(format = ~ style_pvalue(., digits = 3),
      seuil = 0.05) |>
    list_modify(!!!pvalue)

  .font <-
  list(alpha = "bahnschrift",
       digit = "ubuntu") |>
    list_modify(!!!font)

  .palette <-
  list(base = "#999999",
       cold = c("#E1F6FF", "#0099CC"),
       warm = c("#f5E3E0", "#BC3C29")) |>
    list_modify(!!!palette)

  .opts <-
  list(set = list(labs = .labs,
                  sep = .sep,
                  ci = .ci,
                  pvalue = .pvalue,
                  font = .font,
                  palette = .palette))

  assign("opts", .opts, envir = .GlobalEnv)

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
