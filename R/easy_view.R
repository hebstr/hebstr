#' Title
#'
#' @param x arg 
#' @param name arg 
#' @param font_size arg 
#' @param font_family arg 
#' @param strip_color arg 
#' @param assign arg 
#' @param ... arg 
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#' 
easy_view <- \(x,
               name = NULL,
               font_size = "0.8rem",
               font_family = check_fonts(.auto = "luciole"),
               strip_color = set_opts(.assign = FALSE)$color$cold[1],
               assign = FALSE,
               ...) {

  if (is.null(name)) name <- enexpr(x)

  set_cols <- \(y, vars, fn, name) {

    vars <- enexprs(vars)
    fn <- enexpr(fn)

    data <-
    y |>
      select(!!!vars) |>
      names() |>
      set_names() |>
      map(~ eval(fn))

    data <-
    tibble(variable = names(data),
           !!name := data)

  }

  .cols <-
  list(range =
         set_cols(x,
                  vars = where(is.numeric) | where(is.Date),
                  fn = range(x[[.]], na.rm = TRUE) |> round(1),
                  name = "range"),
       q1_q2_q3 =
         set_cols(x,
                  vars = where(is.numeric),
                  fn = quantile(x[[.]],
                                probs = c(.25, .5, .75),
                                na.rm = TRUE) |> round(1),
                  name = "q1_q2_q3"),
       bin =
         set_cols(x,
                  vars = where(is.numeric),
                  fn = length(unique(na.omit(x[[.]]))) == 2,
                  name = "bin"))

  .view <-
  lst(data =
        .cols |>
          reduce(left_join,
                 by = join_by(variable),
                 .init = generate_dictionary(x) |> tibble()) |>
          rename(type = col_type,
                 n_miss = missing) |>
          mutate(type = 
                   case_when(bin == "TRUE" ~ "bin",
                             bin == "FALSE" ~ "num",
                             .default = type),
                 n_miss = na_if(n_miss, 0),
                 q1_q2_q3 = if_else(type == "bin", NA, q1_q2_q3)) |>
          mutate(p_miss = label_p()(n_miss / nrow(x)),
                 .after = n_miss) |>
          relocate(range, q1_q2_q3, .before = levels) |>
          select(where(~ !is.null(unlist(.))), -bin),
      output =
        data |>
          select(-matches(c("range", "q1_q2_q3"))) |>
          rename("n" = pos) |> 
          reactable(defaultExpanded = TRUE,
                    defaultPageSize = 100,
                    showSortable = TRUE,
                    searchable = TRUE,
                    filterable = TRUE,
                    striped = TRUE,
                    resizable = TRUE,
                    defaultColDef = colDef(minWidth = 100),
                    columns = 
                      list("n" = colDef(minWidth = 50),
                           variable = colDef(minWidth = 120),
                           label = colDef(minWidth = 220),
                           type = colDef(minWidth = 60),
                           n_miss = colDef(minWidth = 75),
                           p_miss = colDef(minWidth = 75, align = "right"),
                           levels = colDef(minWidth = 250)),
                    theme = 
                      reactableTheme(style = 
                                       list(fontSize = font_size,
                                            fontFamily = font_family),
                                     stripedColor = strip_color,
                                     searchInputStyle = list(width = "100%")),
                    ...))
  
  if (assign) assign(glue("{name}_view"), .view, envir = .GlobalEnv)
  
  return(.view)

}
