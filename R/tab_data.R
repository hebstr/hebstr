#' Extract metadata from a gt/gtsummary object.
#'
#' @param x Data input. A gt or gtsummary object.
#'
#' @return One or more tibbles
#' @export
#'
#' @examples "arg"
#'
tab_data <- \(x) {

  y <- enexpr(x)

  if (!"gt_tbl" %in% class(x)) {
    
    name <- glue("{y}.gtsum")

    .summary <-
    list(meta =
           x$meta_data |>
             unnest(cols = "df_stats",
                    names_repair = "unique"),
         body = x$table_body,
         style = x$table_styling$header)
  
    .uv_reg <-
    list(meta = x$meta_data,
         data = x$inputs$data %>% filter(.[[2]] != 0))
  
    .body_style <-
    list(body = x$table_body,
         style = x$table_styling$header)
  
    switch(class(x)[1],
           "tbl_summary" = assign(name, .summary, envir = .GlobalEnv),
           "tbl_uvregression" = assign(name, .uv_reg, envir = .GlobalEnv),
           "tbl_regression" = assign(name, .body_style, envir = .GlobalEnv),
           "tbl_merge" = assign(name, .body_style, envir = .GlobalEnv),
           "tbl_strata" = assign(name, .body_style, envir = .GlobalEnv))
  
  } else {
  
    assign(glue("{y}.gt"),
           list(data = x$`_data`,
                header = x$`_boxhead`,
                options = x$`_options`),
           envir = .GlobalEnv)
  
  }

}
