#' Title
#'
#' @param ... A character vector
#' @param replace Replacement pattern. A character vector.
#'
#' @return A named character vector
#' @export
#'
#' @examples "arg"
#'
easy_replace <- \(..., replace = "</>") {
  col_replace <- cli::col_br_red(replace)
  col_replace <- glue("\n\n\n{col_replace}\n\n\n")

  str_list <-
    map(
      c(...),
      ~ list2('{glue("<p>.*({.}).*</p>")}' := replace) |>
        unlist()
    )

  replace_list <- list2("(\n*{replace})+\n*" := col_replace)

  unlist(append(str_list, replace_list))
}


#' Title
#'
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_recode <- \(...) {
  dots <- list(...)

  list(
    name = dots |> map(names) |> unlist(),
    label = dots |> map(unname) |> unlist()
  )
}


#' Title
#'
#' @param x arg
#' @param var arg
#' @param incr arg
#' @param drop arg
#' @param values arg
#' @param labels arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_cut <- \(
  x,
  var,
  incr = FALSE,
  drop = FALSE,
  values = NULL,
  labels = NULL,
  ...
) {
  var <- enexpr(var)

  if (!incr) {
    if (is.null(values)) {
      cli_abort(
        "{.arg values} must be provided when {.arg incr} is {.code FALSE}."
      )
    }

    name <- glue("{var}_cat")

    .min <- min(x[[var]], na.rm = TRUE)

    .max <- max(x[[var]], na.rm = TRUE)

    .values <- map_dbl(values, ~ . - 1 / 10000)

    .labels <- if (is.null(labels)) {
      NULL
    } else {
      str_replace_all(labels, "\\.|,", getOption("OutDec"))
    }

    x <-
      x |>
      mutate(
        !!name := cut(
          x = {{ var }},
          breaks = c(.min - 1, .values, .max + 1),
          labels = .labels,
          right = FALSE
        ),
        .after = all_of(var)
      )
  } else {
    name <- glue("{var}_incr")

    x <-
      x |>
      mutate(
        !!name := cut(x = {{ var }}, breaks = seq(...), right = FALSE) |>
          as.numeric(),
        .after = all_of(var)
      )
  }

  if (drop) {
    x <-
      x |>
      select(-!!var) |>
      rename(!!var := all_of(name))
  }

  return(x)
}


#' Extract PCA variable coordinates, contributions, and weights
#'
#' From a fitted PCA object, builds a tidy summary of the variable-space results:
#' `coord` (rotation loadings, one column per component), `contrib` (squared,
#' column-normalised loadings) and `weight` (first-component contributions
#' rescaled so the maximum is 1). Variable names can optionally be cleaned for
#' display.
#'
#' @param x A fitted PCA object accepted by [broom::tidy()] with
#'   `matrix = "rotation"`, e.g. the result of [stats::prcomp()].
#' @param strip Optional pattern removed from the variable names in the returned
#'   tables, via [stringr::str_remove_all()] (a regular expression). Useful to
#'   drop a shared instrument prefix (e.g. `"hamd"` for Hamilton scale items)
#'   before display. `NULL` (default) leaves the names unchanged.
#'
#' @returns A named list with elements `coord`, `contrib`, and `weight`.
#'
#' @export
#'
#' @examples
#' pca <- prcomp(mtcars, scale = TRUE)
#' pca_var_extract(pca)$weight
#'
pca_var_extract <- \(x, strip = NULL) {
  coord <-
    x |>
    broom::tidy("rotation") |>
    tidyr::pivot_wider(
      names_from = "PC",
      names_prefix = "PC",
      values_from = "value"
    )

  if (!is.null(strip)) {
    coord <- mutate(coord, column = str_remove_all(column, strip))
  }

  lst(
    coord,
    contrib = coord |>
      mutate(across(matches("PC"), ~ .^2 / sum(.^2))),
    weight = coord["column"] |>
      mutate(PC1 = contrib$PC1 / max(contrib$PC1)) |>
      pull(PC1)
  )
}


#' Title
#'
#' @param data arg
#' @param times arg
#' @param method arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_boot <- \(data, times = 1000, method, ...) {
  .f <- \(.) {
    do.call(method, list(parsnip::fit, rsample::analysis(.), ...))
  }

  boot <-
    data |>
    rsample::bootstraps(times = times, apparent = TRUE) |>
    mutate(model = map(splits, .f))

  boot <-
    list(estim_data = tidy, fitted = augment) |>
    map(
      ~ boot |>
        mutate(coef = map(model, .))
    )

  boot <-
    list(
      estimate = list(
        data = boot$estim_data,
        int = boot$estim_data |> int_pctl(coef)
      ),
      fitted = boot$fitted
    )

  return(boot)
}


#' Title
#'
#' @param data arg
#' @param model arg
#' @param y arg
#' @param vars arg
#' @param pv arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
p_picking <- \(data, model, y, vars, pv) {
  fit <- expr(list(reformulate(., y), data = data))

  vars[!vars %in% y] |>
    map(
      ~ do.call(model, eval(fit)) |>
        tidy() |>
        mutate(variable = str_extract(term, str_u(vars)))
    ) |>
    list_rbind() |>
    filter(variable %in% vars, p.value <= pv) |>
    pull(variable)
}


#' Title
#'
#' @param x arg
#' @param column arg
#' @param digits arg
#' @param seuil arg
#' @param table arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
p_shortenr <- \(x, column = p.value, digits = 3, seuil = 0.001, table = TRUE) {
  column <- enexpr(column)

  if (table) {
    inf <- "<"
  } else {
    inf <- "< "
  }
  if (table) {
    sup <- ""
  } else {
    sup <- "= "
  }

  x |>
    rowwise() |>
    mutate(
      !!column := if_else(
        !!column < seuil,
        glue(inf, seuil),
        glue(sup, round(!!column, digits))
      )
    ) |>
    ungroup()
}


#' Title
#'
#' @param data arg
#' @param .var arg
#' @param .min arg
#' @param .fun arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
pct_min <- \(data, .var, .min, .fun = "max") {
  if (!.var %in% names(data)) {
    col <- .var
    cli_abort("{.arg .var} ({.val {col}}) is not a column of {.arg data}.")
  }

  .count <-
    data |>
    count("{.var}" := .data[[.var]]) |>
    mutate(p = n / do.call(.fun, list(n))) |>
    filter(p >= .min) |>
    pull(.var)

  data |>
    filter(.data[[.var]] %in% .count)
}


#' Title
#'
#' @param file arg
#' @param dir arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
read_png <- \(file, dir = "output") {
  glue("{dir}/{file}.png") |>
    readPNG() |>
    rasterGrob()
}


#' Title
#'
#' @param df arg
#' @param y arg
#' @param x arg
#' @param breaks arg
#' @param color arg
#' @param label_y arg
#' @param label_x arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
logit_lty <- \(
  df,
  y,
  x,
  breaks = 40,
  color = "#0099FF",
  label_y = "Logit(P {y})",
  label_x = x
) {
  y <- enexpr(y)
  x <- enexpr(x)

  .outcome <- df[[as_string(y)]]
  if (!is.factor(.outcome) || nlevels(.outcome) != 2) {
    cli_abort(c(
      "{.arg y} must be a 2-level factor; the event is taken as the second level.",
      i = "Convert a 0/1 numeric outcome with {.fun factor} before calling {.fun logit_lty}."
    ))
  }

  lst(
    data = df |>
      mutate("{x}_cat" := cut(!!x, breaks = breaks)) |>
      summarise(
        "mean_{x}" := mean(!!x),
        "prop_{y}" := mean(as.numeric(!!y) == 2),
        logit_prop = log(get(glue("prop_{y}")) / (1 - get(glue("prop_{y}")))),
        .by = glue("{x}_cat")
      ) |>
      filter(!logit_prop %in% c(-Inf, Inf)),
    model = glm(
      data = df |> mutate("{x}_quantile" := cut_number(!!x, n = 4)),
      reformulate(glue("{x}_quantile"), y),
      family = binomial
    ) |>
      tidy(exponentiate = TRUE, conf.int = TRUE) |>
      mutate(p.value = style_pvalue(p.value, digits = 1)) |>
      select(term, estimate, starts_with("conf"), p.value),
    plot = data |>
      ggplot(aes(y = logit_prop, x = get(glue("mean_{x}")))) +
      geom_point(alpha = 0.4) +
      geom_smooth(method = "lm", formula = "y ~ x", se = FALSE, color = color) +
      labs(y = glue(label_y), x = glue(label_x))
  )
}

#' Title
#'
#' @param model arg
#' @param limit_inf_num arg
#' @param limit_sup_num arg
#' @param limit_inf_color arg
#' @param limit_sup_color arg
#' @param obs_color arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
cooksd <- \(
  model,
  limit_inf_num = 4,
  limit_sup_num = 25,
  limit_inf_color = "#0099FF",
  limit_sup_color = "#FF0000",
  obs_color = "#000"
) {
  .out <-
    list(
      n = "{nrow(obs$inf)} total out. for {nrow(data)} total obs.",
      p = "({label_p()(nrow(obs$inf) / nrow(data))})"
    )

  .list <-
    lst(
      data = model |>
        augment() |>
        rownames_to_column("id"),
      limit = c(inf = limit_inf_num, sup = limit_sup_num) |>
        map_dbl(~ . / nrow(data)),
      outliers = limit |>
        map(
          ~ data |>
            filter(.cooksd > .) |>
            pull(id)
        ),
      obs = map(outliers, ~ data[., ]),
      plot = data |>
        ggplot() +
        aes(x = as.numeric(id), y = .cooksd) +
        geom_jitter(color = obs_color, alpha = 0.4) +
        geom_hline(
          yintercept = limit,
          color = c(limit_inf_color, limit_sup_color),
          linewidth = 0.8
        ) +
        annotate(
          geom = "label",
          label = glue(.out$n, .out$p),
          y = max(data$.cooksd),
          x = 1,
          size = 3,
          hjust = 0,
          vjust = 1
        ) +
        xlab(NULL) +
        theme(axis.ticks.x = element_blank(), axis.text.x = element_blank())
    )

  .list[names(.list) != "outliers"]
}


#' Title
#'
#' @param data arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
flow_filter <- \(data, ...) {
  .exprs <- exprs(...)

  if (!is_named(.exprs)) {
    .auto <- map_chr(.exprs, as_label)
    .nms <- names(.exprs)
    .exprs <- set_names(
      .exprs,
      if (is.null(.nms)) .auto else if_else(nzchar(.nms), .nms, .auto)
    )
  }

  .data <- if (inherits(data, c("tbl_sql", "tbl_lazy"))) collect(data) else data

  .flow <-
    .exprs |>
    accumulate(~ filter(.x, !!.y), .init = .data) |>
    map(~ glue("{nrow(.)} ({label_p()(nrow(.) / nrow(.data))})"))

  list(data = .data |> filter(!!!unname(.exprs)), flow = .flow)
}


#' Title
#'
#' @param accuracy arg
#' @param scale arg
#' @param prefix arg
#' @param suffix arg
#' @param big.mark arg
#' @param decimal.mark arg
#' @param trim arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
label_p <- \(
  accuracy = 0.1,
  scale = 100,
  prefix = "",
  suffix = "%",
  big.mark = " ",
  decimal.mark = getOption("OutDec"),
  trim = TRUE,
  ...
) {
  number_format(
    accuracy = accuracy,
    scale = scale,
    prefix = prefix,
    suffix = suffix,
    big.mark = big.mark,
    decimal.mark = decimal.mark,
    trim = trim,
    ...
  )
}

#' Title
#'
#' @param x arg
#' @param to_hash arg
#' @param to_hide arg
#' @param hash_trunc arg
#' @param hash_salt arg
#' @param hide_pattern arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
easy_ano <- \(
  x,
  to_hash = NULL,
  to_hide = NULL,
  hash_trunc = 16,
  hash_salt = NULL,
  hide_pattern = "---"
) {
  .ano_match_cols <- \(patterns) {
    hits <- map(patterns, \(p) str_subset(names(x), glue("^(?:{p})$")))
    missed <- patterns[lengths(hits) == 0]

    if (length(missed) > 0) {
      cli_abort(
        c(
          "No column of {.arg x} matches {.field {missed}}.",
          i = "Patterns match full column names; use a regex (e.g. {.code \"id_.*\"}) to target a family of columns."
        )
      )
    }

    unique(unlist(hits))
  }

  .ano_hash_fun <- \(x_hash, to_hash) {
    x_hash |>
      mutate(
        "{to_hash}" := map_chr(
          get(to_hash),
          \(v) rlang::hash(if (is.null(hash_salt)) v else paste0(hash_salt, v))
        ) |>
          str_remove_all(glue(".{{{hash_trunc}}}$"))
      )
  }

  .ano_hide_fun <- \(x_hide) {
    x_hide |>
      mutate(across(all_of(.ano_match_cols(to_hide)), ~hide_pattern))
  }

  if (!is.null(to_hash)) {
    .ano_data <-
      .ano_match_cols(to_hash) |>
      reduce(.ano_hash_fun, .init = x)

    if (!is.null(to_hide)) {
      .ano_data <- .ano_hide_fun(.ano_data)
    }
  } else if (!is.null(to_hide)) {
    .ano_data <- .ano_hide_fun(x)
  } else {
    .ano_data <- x
  }

  return(.ano_data)
}

#' Title
#'
#' @param x arg
#' @param sheet arg
#' @param ... arg
#' @param data arg
#' @param max_width arg
#' @param halign arg
#' @param font_size arg
#' @param font_color arg
#' @param concept_var arg
#' @param concept_color arg
#' @param text_var arg
#' @param text_color arg
#' @param border_color arg
#' @param border_type arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
wb_add_custom <- \(
  x,
  sheet,
  ...,
  data,
  max_width = 100,
  halign = "center",
  font_size = 8,
  font_color = "#222222",
  concept_var = NULL,
  concept_color = NULL,
  text_var = NULL,
  text_color = NULL,
  border_color = "#999999",
  border_type = "thin"
) {
  local_options(list(openxlsx2.maxWidth = max_width))

  .dims <-
    list(
      full = wb_dims(x = data),
      data = wb_dims(x = data, select = "data"),
      cols = wb_dims(x = data, select = "col_names")
    )

  .colors <-
    list(
      border = wb_color(border_color),
      header = wb_color("grey90")
    )

  .xlsx_output <-
    x |>
    wb_add_worksheet(
      sheet = sheet,
      zoom = 105,
      ...
    ) |>
    wb_add_data_table(
      x = data,
      na.strings = NULL
    ) |>
    wb_add_font(
      dims = .dims$cols,
      size = font_size + 1,
      bold = TRUE
    ) |>
    wb_add_font(
      dims = .dims$data,
      size = font_size
    ) |>
    wb_add_fill(
      dims = .dims$cols,
      color = .colors$header
    ) |>
    wb_set_col_widths(
      cols = seq_len(ncol(data)),
      widths = "auto"
    ) |>
    wb_add_cell_style(
      dims = .dims$full,
      horizontal = halign,
      vertical = "center",
      wrap_text = TRUE
    ) |>
    wb_add_border(
      dims = .dims$data,
      top_color = .colors$border,
      top_border = border_type,
      bottom_color = .colors$border,
      bottom_border = border_type,
      left_color = .colors$border,
      left_border = border_type,
      right_color = .colors$border,
      right_border = border_type,
      inner_hcolor = .colors$border,
      inner_hgrid = border_type,
      inner_vcolor = .colors$border,
      inner_vgrid = border_type
    )

  .add_font <- \(wb, vars, color) {
    .dims <- wb_dims(x = data, cols = vars, select = "data")
    .color <- wb_color(color)

    wb_add_font(
      wb = wb,
      dims = .dims,
      color = .color,
      size = font_size,
      bold = TRUE
    )
  }

  if (!is.null(concept_color)) {
    .xlsx_output <-
      .add_font(wb = .xlsx_output, vars = concept_var, color = concept_color)
  }

  if (!is.null(text_color)) {
    .xlsx_output <-
      .add_font(wb = .xlsx_output, vars = text_var, color = text_color)
  }

  return(.xlsx_output)
}
