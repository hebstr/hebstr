#' Title
#'
#' @param x
#' @param title
#' @param note_global
#' @param note_pvalue
#' @param label_vargrp
#' @param note_vargrp
#' @param acro_list
#' @param acro_sep
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
gt_format <- \(x,
               title = NULL,
               note_global = NULL,
               note_pvalue = NULL,
               label_vargrp = NULL,
               note_vargrp = NULL,
               acro_list,
               acro_sep,
               ...) {

### ACRO --------------------------------------------------------------------

  style <- x$table_styling$header$label

  body <-
  x$table_body |> 
    names() |>
    str_subset(".*label") |>
    map(~ x$table_body[[.]]) |>
    unlist()

  .acro <- acro_extract(c(style, body, names(x)), acro_list)

### THEME -------------------------------------------------------------------

  if (!"gt_tbl" %in% class(x)) x <- as_gt(x)

  x <-
  x |> 
    tab_header(title = if (!is.null(title)) md(title)) |> 
    theme_gt(...)
  
  if (TRUE %in% str_starts(names(x[["_data"]]), "coef")) {
    
    .acro_str <-
    acro_str(.estim$uv,
             .estim$mv,
             with(acro_list, mget(.acro[.acro != "N"])),
             collapse = acro_sep)
    
    if (!is.null(note_global) || !is.null(.acro_str)) {
    
      x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))
    
    }
    
    if (!is.null(note_pvalue)) {
      
      x <-
      tab_footnote(x,
                   footnote = note_pvalue,
                   locations = cells_column_labels(p.value_2))
      
    }
    
  } else {
    
    .acro_str <-
    acro_str(with(acro_list, mget(.acro)),
             collapse = acro_sep)
    
    if (!is.null(note_global) || !is.null(.acro_str)) {
    
      x <- tab_footnote(x, footnote = c(str_c(note_global), .acro_str))
     
    }
    
    if (!is.null(note_vargrp)) {
     
      x <-
      tab_footnote(x,
                   footnote = note_vargrp,
                   locations = 
                     cells_body(columns = label,
                                rows = variable %in% label_vargrp))
      
    }
    
  }
  
  return(x)

}
