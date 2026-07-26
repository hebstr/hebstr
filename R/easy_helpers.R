#' Build named regex replacements collapsing HTML paragraphs to a token
#'
#' For each input string, produces a regex that matches it inside an HTML
#' paragraph and maps it to `replace`, then adds a rule collapsing runs of the
#' token into a single coloured marker. The result feeds
#' [stringr::str_replace_all()] as its `pattern`/`replacement` mapping.
#'
#' @param ... A character vector
#' @param replace The literal token each match is replaced with, inserted as is
#'   and escaped before it enters the collapsing rule. A character vector.
#'
#' @return A named character vector
#' @export
#'
#' @examples
#' easy_replace("hello", "world")
#' easy_replace("age", "sex", replace = "[X]")
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

  replace_list <- list2("(\n*{str_escape(replace)})+\n*" := col_replace)

  unlist(append(str_list, replace_list))
}


#' Split named arguments into parallel name and label vectors
#'
#' @param ... Named values, where the names supply codes and the values supply
#'   labels.
#'
#' @return A list with two character vectors: `name` (the argument names) and
#'   `label` (the argument values).
#' @export
#'
#' @examples
#' easy_recode(m = "Male", f = "Female")
#' easy_recode(c(a = "Label A"), c(b = "Label B", c = "Label C"))
#'
easy_recode <- \(...) {
  dots <- list(...)

  list(
    name = dots |> map(names) |> unlist(),
    label = dots |> map(unname) |> unlist()
  )
}


#' Bin a numeric column into categories
#'
#' Adds a categorical version of a numeric column, either from explicit
#' breakpoints or from equal-width increments.
#'
#' @param x A data frame.
#' @param var The numeric column to bin (unquoted).
#' @param incr Whether to bin into equal-width increments defined by `...`
#'   (passed to [seq()]). When `FALSE`, `values` supplies the breakpoints.
#' @param drop Whether to drop the original column and rename the binned column
#'   to the original name.
#' @param values Breakpoints for the categories; required when `incr` is
#'   `FALSE`.
#' @param labels Labels for the resulting categories; decimal marks are
#'   localised via `getOption("OutDec")`. `NULL` uses the default interval
#'   labels.
#' @param ... Arguments forwarded to [seq()] (e.g. `by`) when `incr` is `TRUE`.
#'
#' @return `x` with an added categorical column (`{var}_cat` or `{var}_incr`),
#'   or with the original column replaced when `drop` is `TRUE`.
#' @export
#'
#' @examples
#' penguins <- na.omit(datasets::penguins)
#' easy_cut(penguins, body_mass, values = c(3500, 4500))
#' easy_cut(penguins, flipper_len, incr = TRUE, from = 170, to = 240, by = 10)
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


#' Bootstrap a model and summarize coefficients
#'
#' Resamples the data with [rsample::bootstraps()], fits `method` on each
#' analysis set, and collects tidied coefficients and augmented predictions.
#'
#' @param data A data frame to resample.
#' @param times Number of bootstrap resamples.
#' @param method A modelling function passed to [parsnip::fit()] on each
#'   resample's analysis set.
#' @param ... Additional arguments forwarded to `method`.
#'
#' @return A named list with `estimate` (a list of `data`, the per-resample
#'   tidied coefficients, and `int`, their percentile intervals from
#'   [rsample::int_pctl()]) and `fitted` (the per-resample augmented
#'   predictions).
#' @export
#'
#' @examples
#' \donttest{
#' df <- data.frame(x = seq_len(30), y = seq_len(30) + rnorm(30))
#' lm_boot <- \(fit, data) lm(y ~ x, data = data)
#' boot <- easy_boot(df, times = 25, method = lm_boot)
#' boot$estimate$int
#' }
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


#' Select variables by univariate p-value threshold
#'
#' Fits `model` for each candidate variable against `y` and keeps those whose
#' univariate p-value is at or below `pv`.
#'
#' @param data A data frame.
#' @param model A modelling function called via [do.call()] (e.g. [stats::glm]).
#' @param y The outcome variable name, used in [stats::reformulate()].
#' @param vars Candidate predictor variable names.
#' @param pv The p-value threshold for retaining a variable.
#'
#' @return A character vector of the variable names meeting the threshold.
#' @export
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   y = factor(rep(c("A", "B"), length.out = 60)),
#'   x = rnorm(60),
#'   g = factor(rep(c("g1", "g2", "g3"), length.out = 60))
#' )
#' p_picking(
#'   d,
#'   \(formula, data) glm(formula, data, family = binomial),
#'   "y",
#'   c("x", "g"),
#'   pv = 0.5
#' )
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


#' Format a p-value column with a threshold cutoff
#'
#' Rewrites a p-value column as text: values below `seuil` become a `<` cutoff
#' string, and values at or above it are rounded.
#'
#' @param x A data frame.
#' @param column The column to format (unquoted; defaults to `p.value`).
#' @param digits Rounding digits for values at or above `seuil`.
#' @param seuil Threshold below which values are shown with the `<` prefix.
#' @param table Whether to use compact table formatting (no spaces); `FALSE`
#'   adds spacing and an `=` prefix for inline text.
#'
#' @return `x` with `column` rewritten as formatted character strings.
#' @export
#'
#' @examples
#' p_values <- data.frame(p.value = c(0.0001, 0.03, 0.5))
#' p_shortenr(p_values)
#' p_shortenr(p_values, table = FALSE)
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


#' Keep rows in frequent-enough categories
#'
#' Retains rows whose category reaches a minimum share of the counts, where the
#' denominator is `.fun` applied to the per-category counts.
#'
#' @param data A data frame.
#' @param .var The column name (string) whose category frequencies are
#'   evaluated.
#' @param .min Minimum proportion a category must reach to be kept.
#' @param .fun Function applied to the counts to form the denominator (default
#'   `"max"`).
#'
#' @return `data` filtered to rows in categories meeting the proportion
#'   threshold.
#' @export
#'
#' @examples
#' penguins <- na.omit(datasets::penguins)
#' pct_min(penguins, "island", 0.5)
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


#' Read a PNG into a raster grob
#'
#' @param file PNG file name without extension.
#' @param dir Directory containing the file.
#'
#' @return A [grid::rasterGrob()] object.
#' @export
#'
#' @examples
#' \donttest{
#' dir <- tempdir()
#' png::writePNG(array(runif(300), dim = c(10, 10, 3)), file.path(dir, "demo.png"))
#' img <- read_png("demo", dir = dir)
#' }
#'
read_png <- \(file, dir = "output") {
  glue("{dir}/{file}.png") |>
    readPNG() |>
    rasterGrob()
}


#' Logit linearity check for a numeric predictor
#'
#' Compares empirical logits of a binary outcome across bins of a numeric
#' predictor against a linear fit, alongside a logistic model on the
#' predictor's quartiles.
#'
#' @param df A data frame.
#' @param y The outcome, a 2-level factor with the event taken as the second
#'   level (unquoted). A non-factor or non-binary outcome aborts.
#' @param x The numeric predictor (unquoted).
#' @param breaks Number of bins used to compute the empirical logits.
#' @param color Colour of the fitted line.
#' @param label_y The y-axis label ([glue::glue()] template).
#' @param label_x The x-axis label ([glue::glue()] template; defaults to `x`).
#'
#' @return A named list with `data` (per-bin mean `x`, event proportion, and
#'   empirical logit), `model` (a tidied tibble of `term`, `estimate`,
#'   `conf.low`, `conf.high`, and `p.value` from an exponentiated logistic fit
#'   on `x` quartiles), and `plot` (a ggplot of empirical logit against mean
#'   `x` with a linear fit).
#' @export
#'
#' @examples
#' d <- data.frame(
#'   xv = seq_len(120),
#'   yv = factor(rep(c("no", "yes"), length.out = 120))
#' )
#' res <- logit_lty(d, yv, xv, breaks = 6)
#' res$model
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

#' Cook's distance outlier diagnostics
#'
#' Augments a model with Cook's distance, flags observations exceeding two
#' cutoffs (each a numerator divided by the number of observations), and plots
#' the distances with the cutoff lines.
#'
#' @param model A fitted model accepted by [broom::augment()].
#' @param limit_inf_num Numerator of the lower cutoff (cutoff = `limit_inf_num`
#'   divided by the number of observations).
#' @param limit_sup_num Numerator of the upper cutoff.
#' @param limit_inf_color Colour of the lower threshold line.
#' @param limit_sup_color Colour of the upper threshold line.
#' @param obs_color Colour of the plotted observation points.
#'
#' @return A named list with `data` (the augmented observations with `.cooksd`),
#'   `limit` (the two cutoff values), `obs` (data frames of observations
#'   exceeding each cutoff), and `plot` (a ggplot of Cook's distance with the
#'   threshold lines).
#' @export
#'
#' @examples
#' res <- cooksd(lm(mpg ~ wt, mtcars))
#' res$obs
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


#' Cumulative filter with a row-count flow log
#'
#' Applies filtering expressions one after another and records how many rows
#' remain after each step, useful for documenting an inclusion flow.
#'
#' @param data A data frame, or a lazy/SQL table (collected first).
#' @param ... Filtering expressions applied cumulatively; names label each step
#'   and are auto-derived from the expression when unnamed.
#'
#' @return A named list with `data` (the data filtered by all expressions) and
#'   `flow` (the remaining count and percentage after each successive filter).
#' @export
#'
#' @examples
#' penguins <- na.omit(datasets::penguins)
#' res <- flow_filter(penguins, bill_len > 40, species == "Adelie")
#' res$flow
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


#' Percentage label formatter
#'
#' Wraps [scales::number_format()] with defaults suited to percentages,
#' respecting the locale's decimal mark.
#'
#' @param accuracy Rounding accuracy.
#' @param scale Multiplier applied before formatting (`100` expresses
#'   proportions as percentages).
#' @param prefix String prepended to each label.
#' @param suffix String appended to each label.
#' @param big.mark Thousands separator.
#' @param decimal.mark Decimal separator (localised via `getOption("OutDec")`).
#' @param trim Whether to trim leading whitespace.
#' @param ... Further arguments forwarded to [scales::number_format()].
#'
#' @return A labelling function from [scales::number_format()].
#' @export
#'
#' @examples
#' fmt <- label_p()
#' fmt(c(0.1, 0.256))
#' label_p(accuracy = 1)(0.256)
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

#' Anonymize columns by hashing or masking
#'
#' Hashes selected columns with [rlang::hash()] (optionally salted, then
#' truncated) and overwrites others with a fixed placeholder. Column selectors
#' are regexes matched against full column names.
#'
#' @param x A data frame to anonymize.
#' @param to_hash Regex patterns matching, on full column names, the columns to
#'   hash.
#' @param to_hide Regex patterns matching the columns to overwrite with
#'   `hide_pattern`.
#' @param hash_trunc Number of trailing hash characters dropped from each hash.
#' @param hash_salt Optional salt string prepended to each value before
#'   hashing.
#' @param hide_pattern Replacement string written into hidden columns.
#'
#' @returns `x` with the matched columns hashed and/or masked.
#' @export
#'
#' @examples
#' penguins <- na.omit(datasets::penguins)
#' easy_ano(penguins, to_hash = "species", to_hide = "island")
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

#' Add a styled data table to a workbook worksheet
#'
#' Writes a data frame to a new worksheet with header shading, auto-fitted
#' columns, wrapped and aligned cells, uniform borders, and optional per-column
#' font colours.
#'
#' @param x A workbook object (`wbWorkbook`).
#' @param sheet Worksheet name.
#' @param ... Arguments forwarded to [openxlsx2::wb_add_worksheet()].
#' @param data The data frame written as a table.
#' @param max_width Maximum auto-fit column width (`openxlsx2.maxWidth`).
#' @param halign Horizontal cell alignment.
#' @param font_size Base font size; the header row is one point larger.
#' @param font_color Font colour for the table body.
#' @param concept_var Columns recoloured with `concept_color`.
#' @param concept_color Font colour applied to `concept_var` when non-`NULL`.
#' @param text_var Columns recoloured with `text_color`.
#' @param text_color Font colour applied to `text_var` when non-`NULL`.
#' @param border_color Cell border colour.
#' @param border_type Border style (e.g. `"thin"`).
#'
#' @returns The modified workbook object (`wbWorkbook`).
#' @export
#'
#' @examples
#' \donttest{
#' wb <- openxlsx2::wb_workbook()
#' wb <- wb_add_custom(wb, sheet = "cars", data = head(mtcars))
#' }
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
