#' Variable dictionary of a data frame
#'
#' Summarises each variable of a data frame (type, missingness, range, quartiles,
#' factor levels) and returns that dictionary three ways: as a tibble, as an
#' interactive [reactable::reactable()] widget, and, on demand, as a
#' JSON-friendly copy.
#'
#' @param x A data frame to summarise.
#' @param cols <[`tidy-select`][dplyr::dplyr_tidy_select]> Columns of the
#'   summary to show in the widget, selected on their displayed names (`n`,
#'   `variable`, `label`, `type`, `n_miss`, `p_miss`, `range`, `q1_med_q3`,
#'   `levels`) rather than on the `pos` the returned `data` carries. Defaults to
#'   every column of the summary; a column the frame does not carry (`range` on
#'   a frame with no numeric column, say) is absent from the selection too.
#' @param font_size CSS font size for the widget's text.
#' @param font_family Font family for the widget's text. When [set_opts()] has
#'   been called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param strip_color Background color of the striped rows. Defaults to the first
#'   cold-palette color from the options.
#' @param json Also return a JSON-friendly copy of the summary? It keeps the
#'   multi-valued cells as list columns marked with [base::I()], so that a JSON
#'   writer emits arrays rather than strings, counts missing values as `0`
#'   instead of `NA` and carries `p_miss` as a proportion rather than a
#'   formatted percentage.
#' @param ... Additional arguments forwarded to [reactable::reactable()].
#'
#' @return A named list with `data` (the per-variable summary tibble), `output`
#'   (the interactive `reactable` widget) and `json` (the JSON-friendly copy, or
#'   `NULL`). `data` flattens the multi-valued cells (`range`, `q1_med_q3`,
#'   `levels`) into `" ; "`-separated strings.
#' @export
#'
#' @examples
#' \dontrun{
#' get_vars_dict(datasets::penguins)
#' get_vars_dict(datasets::penguins, cols = c(n, variable, type, n_miss))
#' }
#'
get_vars_dict <- \(
  x,
  cols = everything(),
  font_size = "0.7rem",
  font_family = .text_font(),
  strip_color = set_opts(.assign = FALSE)$color$cold[1],
  json = FALSE,
  ...
) {
  set_cols <- \(y, vars, fn, name) {
    vars <- enexprs(vars)
    fn <- enexpr(fn)

    data <- y |>
      select(!!!vars) |>
      names() |>
      set_names() |>
      map(~ eval(fn))

    data <- tibble(variable = names(data), !!name := data)
  }

  .data_cols <- list(
    range = set_cols(
      x,
      vars = where(is.numeric) | where(is.Date),
      fn = range(x[[.]], na.rm = TRUE) |> round(1),
      name = "range"
    ),
    q1_med_q3 = set_cols(
      x,
      vars = where(is.numeric),
      fn = x[[.]] |>
        quantile(probs = c(0.25, 0.5, 0.75), na.rm = TRUE) |>
        round(1),
      name = "q1_med_q3"
    ),
    bin = set_cols(
      x,
      vars = where(is.numeric),
      fn = length(unique(na.omit(x[[.]]))) == 2,
      name = "bin"
    )
  )

  .dict <- .data_cols |>
    reduce(
      left_join,
      by = join_by(variable),
      .init = x |> generate_dictionary() |> tibble()
    ) |>
    rename(type = col_type, n_miss = missing) |>
    mutate(
      type = case_when(
        bin == "TRUE" ~ "bin",
        bin == "FALSE" ~ "num",
        .default = type
      ),
      n_miss = na_if(n_miss, 0),
      q1_med_q3 = if_else(type == "bin", NA, q1_med_q3)
    ) |>
    mutate(p_miss = label_p()(n_miss / nrow(x)), .after = n_miss) |>
    relocate(range, q1_med_q3, .before = levels) |>
    select(where(~ !is.null(unlist(.))), -bin)

  .view_data <- .dict |>
    mutate(across(where(is.list), \(col) {
      map_chr(col, ~ str_flatten(.x, " ; ")) |> na_if("")
    }))

  .data_json <- if (json) {
    .dict |>
      mutate(
        n_miss = coalesce(n_miss, 0L),
        p_miss = n_miss / nrow(x),
        across(where(is.list), \(col) {
          map(col, ~ if (is.null(.x)) NA else I(.x))
        })
      )
  }

  .output_cols <- .view_data |>
    rename("n" = pos) |>
    select(!!enexpr(cols))

  if (!ncol(.output_cols)) {
    cli_abort("{.arg cols} must select at least one column of the summary.")
  }

  .view_output <- .output_cols |>
    reactable(
      defaultExpanded = TRUE,
      defaultPageSize = 100,
      showSortable = TRUE,
      searchable = TRUE,
      filterable = TRUE,
      striped = TRUE,
      resizable = TRUE,
      columns = .col_defs(
        .output_cols,
        font_size,
        extra = list(n_miss = list(align = "left"))
      ),
      theme = reactableTheme(
        style = list(fontSize = font_size, fontFamily = font_family),
        stripedColor = strip_color,
        searchInputStyle = list(width = "100%")
      ),
      ...
    )

  .view <- lst(
    data = .view_data,
    output = .view_output,
    json = .data_json
  )

  return(.view)
}

.font_px <- \(size, root = 16) {
  value <- as.numeric(str_extract(size, "[0-9.]+"))

  if (is.na(value)) {
    return(root)
  }

  switch(
    str_extract(size, "[a-z%]+$"),
    px = value,
    pt = value * 4 / 3,
    `%` = value * root / 100,
    value * root
  )
}

.col_defs <- \(
  data,
  font_size,
  extra = list(),
  em = 0.47,
  pad = 20,
  bounds = c(50, 300)
) {
  char <- .font_px(font_size) * em

  imap(data, \(col, name) {
    chars <- max(nchar(as.character(col)), nchar(name) + 3, na.rm = TRUE)

    inject(colDef(
      minWidth = round(min(max(chars * char + pad, bounds[1]), bounds[2])),
      !!!extra[[name]]
    ))
  })
}
