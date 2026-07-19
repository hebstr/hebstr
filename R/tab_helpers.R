#' Parametric test for a numeric variable across groups
#'
#' Custom test compatible with the gtsummary `add_p()` interface. Runs a
#' Student t-test when `by` has two levels and a one-way ANOVA otherwise, both
#' assuming equal variances.
#'
#' @param data A data frame holding `variable` and `by`.
#' @param variable String naming the numeric column to test.
#' @param by String naming the grouping column.
#' @param ... Unused; accepted for compatibility with the gtsummary custom-test
#'   interface.
#' @return A one-row tibble from [broom::tidy()] with the test statistics and
#'   p-value.
#' @export
#'
#' @examples
#' pen <- na.omit(datasets::penguins)
#'
#' # Two groups: Student t-test
#' quanti.test.para(pen, "body_mass", "sex")
#'
#' # Three groups: one-way ANOVA
#' quanti.test.para(pen, "body_mass", "species")
#'
quanti.test.para <- \(data, variable, by, ...) {
  .var <- data[[variable]]
  .by <- data[[by]]

  if (nlevels(factor(.by)) == 2) {
    tidy(t.test(.var ~ .by, var.equal = TRUE))
  } else {
    tidy(oneway.test(.var ~ .by, var.equal = TRUE))
  }
}


#' Non-parametric test for a numeric variable across groups
#'
#' Custom test compatible with the gtsummary `add_p()` interface. Runs a
#' Wilcoxon rank-sum test when `by` has two levels and a Kruskal-Wallis test
#' otherwise.
#'
#' @param data A data frame holding `variable` and `by`.
#' @param variable String naming the numeric column to test.
#' @param by String naming the grouping column.
#' @param ... Unused; accepted for compatibility with the gtsummary custom-test
#'   interface.
#' @return A one-row tibble from [broom::tidy()] with the test statistics and
#'   p-value.
#' @export
#'
#' @examples
#' pen <- na.omit(datasets::penguins)
#'
#' # Two groups: Wilcoxon rank-sum test
#' quanti.test.nonpara(pen, "body_mass", "sex")
#'
#' # Three groups: Kruskal-Wallis test
#' quanti.test.nonpara(pen, "body_mass", "species")
#'
quanti.test.nonpara <- \(data, variable, by, ...) {
  .var <- data[[variable]]
  .by <- data[[by]]

  if (nlevels(factor(.by)) == 2) {
    tidy(wilcox.test(.var ~ .by, exact = FALSE, correct = FALSE))
  } else {
    tidy(kruskal.test(.var ~ .by))
  }
}


#' Categorical association test with automatic Fisher fallback
#'
#' Custom test compatible with the gtsummary `add_p()` interface. Uses a
#' chi-squared test without continuity correction when all expected counts are
#' at least 5, applies the continuity correction when the smallest expected
#' count falls between 2 and 5, and switches to Fisher's exact test when any
#' expected count drops below 2.
#'
#' @param data A data frame holding `variable` and `by`.
#' @param variable String naming the categorical column to test.
#' @param by String naming the grouping column.
#' @param ... Unused; accepted for compatibility with the gtsummary custom-test
#'   interface.
#' @return A one-row tibble from [broom::tidy()] with the test statistics and
#'   p-value.
#' @export
#'
#' @examples
#' pen <- na.omit(datasets::penguins)
#'
#' # Balanced cross-table: chi-squared without continuity correction
#' quali.test(pen, "sex", "species")
#'
quali.test <- \(data, variable, by, ...) {
  .var <- data[[variable]]
  .by <- factor(data[[by]])

  chisq_is_correct <- \(correct) {
    chisq.test(.var, .by, correct = correct) |>
      suppressWarnings()
  }

  chisq.test.no.correct <- chisq_is_correct(FALSE)

  is_under <- \(n) {
    chisq.test.no.correct$expected |>
      data.frame() |>
      filter(if_any(everything(), ~ . < n)) |>
      nrow() >
      0
  }

  if (is_under(5)) {
    if (!is_under(2)) {
      tidy(chisq_is_correct(TRUE))
    } else {
      tidy(fisher.test(.var, .by))
    }
  } else {
    tidy(chisq.test.no.correct)
  }
}


#' Names of dichotomous variables
#'
#' Identifies the columns that are factors with exactly two levels.
#'
#' @param data A data frame to inspect.
#' @param ... Optional column names to restrict the search to; defaults to all
#'   columns of `data`.
#' @return A character vector of the column names with exactly two levels.
#' @export
#'
#' @examples
#' # Only `sex` is a two-level factor
#' all_dichotomous_uv(datasets::penguins)
#'
#' # Restrict the search to named columns
#' all_dichotomous_uv(datasets::penguins, "sex", "species")
#'
all_dichotomous_uv <- \(data, ...) {
  dots <- c(...) %||% names(data)

  level <- map_int(dots, ~ nlevels(data[[.]]))

  dots[level == 2]
}


#' Attach a footnote to targeted rows or a p-value column
#'
#' Adds a footnote to a `gtsummary` table, targeting either specific label rows
#' or a multivariable p-value column. Exactly one of `vars`, `levels`, `rows`,
#' or `pvalue_mv` selects the location.
#'
#' Footnotes are declared on the `gtsummary` object, upstream of [tbl_format()],
#' which renders to a `gt_tbl` or to a `flextable` under
#' `options(hebstr.docx = TRUE)`. Declaring them upstream keeps a single call
#' site across both output formats, and leaves `gtsummary` to number the symbols
#' in table reading order regardless of the order the notes are declared in.
#'
#' @param data A `gtsummary` table object, upstream of [tbl_format()].
#' @param vars Variable name(s) to footnote on their label rows.
#' @param levels Level label(s) to footnote on the matching rows.
#' @param rows A data-mask expression selecting the rows to footnote.
#' @param pvalue_mv Zero-based index of a multivariable p-value column; footnotes
#'   the `p.value_{pvalue_mv + 1}` column label instead of body rows.
#' @param note Footnote text to display.
#' @return A `gtsummary` table with the footnote added.
#' @export
#'
#' @examples
#' tbl <-
#'   na.omit(datasets::penguins) |>
#'   gtsummary::tbl_summary(include = c(species, bill_len)) |>
#'   add_note(vars = "species", note = "Counts by species.")
#'
add_note <- \(
  data,
  vars = NULL,
  levels = NULL,
  rows = NULL,
  pvalue_mv = NULL,
  note
) {
  if (!inherits(data, "gtsummary")) {
    cli_abort(
      c(
        "{.arg data} must be a {.cls gtsummary} table, not {.obj_type_friendly {data}}.",
        i = "Call {.fun add_note} before {.fun tbl_format}, which renders the table to its final class."
      )
    )
  }

  if (!is.null(pvalue_mv)) {
    return(modify_footnote_header(
      data,
      footnote = note,
      columns = glue("p.value_{pvalue_mv + 1}")
    ))
  }

  .rows <- enquo(rows)

  if (!is.null(vars)) {
    .rows <- expr(variable %in% !!vars & row_type == "label")
  } else if (!is.null(levels)) {
    .rows <- expr(label %in% !!levels)
  } else if (quo_is_null(.rows)) {
    cli_abort(
      "Provide at least one of {.arg vars}, {.arg levels}, or {.arg rows} to target the footnote to specific rows."
    )
  }

  inject(modify_footnote_body(
    data,
    footnote = note,
    columns = "label",
    rows = !!.rows
  ))
}


#' Format factor counts as a single string
#'
#' Counts the levels of `x` in decreasing frequency and flattens them into one
#' string of `level [n]` entries.
#'
#' @param x A factor or character vector to tabulate.
#' @param sep Separator inserted between the `level [n]` entries.
#' @param cap Whether to capitalize the first letter of the result.
#' @return A character string listing each level with its count.
#' @export
#'
#' @examples
#' fct_str(datasets::penguins$island, sep = ", ")
#'
#' # Keep the original casing
#' fct_str(datasets::penguins$island, sep = " | ", cap = FALSE)
#'
fct_str <- \(x, sep, cap = TRUE) {
  str <-
    x |>
    str_squish() |>
    fct_count(sort = TRUE) |>
    drop_na() |>
    mutate(str = glue("{f} [{n}]")) |>
    pull(str) |>
    str_flatten(sep) |>
    glue(".")

  if (cap) {
    str <- str |> tolower() %>% str_cap(toupper, .)
  }

  return(str)
}


#' Format rare factor levels together with a companion vector
#'
#' Lists the levels of `fct` that fall below `min` (excluding `"Other"`) as
#' `level (n)` entries, then appends the [fct_str()] tally of `chr`.
#'
#' @param fct A factor whose rare levels are listed.
#' @param chr A factor or character vector appended via [fct_str()].
#' @param min Frequency threshold; levels of `fct` with fewer counts are listed.
#' @param sep Separator passed to [fct_str()] for the `chr` tally.
#' @return A character string combining the rare `fct` levels and the `chr`
#'   counts.
#' @export
#'
#' @examples
#' # Species below 100 counts (Chinstrap) listed, then island tally appended
#' fct_other_str(
#'   fct = datasets::penguins$species,
#'   chr = datasets::penguins$island,
#'   min = 100
#' )
#'
fct_other_str <- \(fct, chr, min, sep = ", ") {
  fct <-
    fct |>
    fct_count(sort = TRUE) |>
    filter(f != "Other", n < min) |>
    mutate(str = glue("{f} ({n})")) |>
    pull(str) |>
    str_flatten_comma() |>
    tolower()

  fct <- str_cap(toupper, fct)

  chr <- fct_str(chr, sep, cap = FALSE)

  parts <- c(fct, chr)
  paste(parts[nzchar(parts)], collapse = ", ")
}

#' Split, count, and partition a delimited variable by frequency
#'
#' Splits `var` on commas or `"et"`, counts the resulting values, then
#' partitions them into frequent values to keep and rare values to drop.
#'
#' @param data A data frame holding `var`.
#' @param var Name of the delimited column to split and tally.
#' @param min Frequency threshold; values with at least `min` counts are kept.
#' @param sep Separator used to flatten the dropped `value [n]` entries.
#' @return A list with `keep` (character vector of frequent values) and `drop`
#'   (a string listing the rare values with their counts).
#' @export
#'
#' @examples
#' df <- data.frame(
#'   drugs = c("aspirin, ibuprofen", "aspirin et codeine", "ibuprofen", "aspirin")
#' )
#'
#' # Keep values seen at least twice, list the rarer ones
#' fct_keep(df, "drugs", min = 2, sep = ", ")
#'
fct_keep <- \(data, var, min, sep) {
  tab <-
    data |>
    separate_longer_delim(!!var, delim = regex("\\s*(,|et)\\s*")) |>
    count(!!var := get(var), sort = TRUE) |>
    drop_na()

  x <-
    tab |>
    split(factor(
      tab$n >= min,
      levels = c(FALSE, TRUE),
      labels = c("drop", "keep")
    ))

  y <-
    list(
      keep = x$keep |>
        pull(!!var) |>
        as.character(),
      drop = x$drop |>
        mutate(str = glue("{get(var)} [{n}]")) |>
        pull(str) |>
        str_flatten(sep) %>%
        glue(".")
    )

  return(y)
}


#' Insert a grouping header row above a set of variables
#'
#' Adds a header row labeled `name` before the first of `levels`, then indents
#' the header and nests the grouped rows beneath it.
#'
#' @details The inserted row carries `NA` in every column but `label`,
#'   including `variable` and `row_type`. Row predicates applied downstream must
#'   tolerate it: prefer `row_type %in% "label"` over `row_type == "label"`.
#'
#' @param x A `gtsummary` table object.
#' @param name Label of the inserted header row.
#' @param levels Variable names grouped under the header, in table order.
#' @param indent Indentation applied to the header row; the grouped rows are
#'   indented by `indent + 4`.
#' @return A `gtsummary` table with the header row inserted and rows indented.
#' @export
#'
#' @examples
#' tbl <-
#'   na.omit(datasets::penguins) |>
#'   gtsummary::tbl_summary(include = c(species, island, sex)) |>
#'   add_label(name = "Categorical", levels = c("species", "island"))
#'
add_label <- \(x, name, levels, indent = 0) {
  .before_index <- match(levels[1], x$table_body$variable)

  x <-
    x |>
    modify_table_body(
      ~ . |>
        add_row(label = name, .before = .before_index)
    ) |>
    modify_indent(
      columns = label,
      rows = label == name,
      indent = indent
    ) |>
    modify_indent(
      columns = label,
      rows = variable %in% levels,
      indent = indent + 4
    )
}


#' Missing-data summary sentence
#'
#' Builds a sentence reporting the total number of observations and how many
#' contain at least one missing value, with the associated percentage.
#'
#' @param data A data frame to summarize.
#' @return A character string describing the count of rows with missing data.
#' @export
#'
#' @examples
#' # `penguins` carries missing values; keep them to summarize
#' str_na_mv(datasets::penguins)
#'
str_na_mv <- \(data) {
  n_total <- nrow(data)

  na <-
    lst(
      n = data |> filter(if_any(everything(), is.na)) |> nrow(),
      p = label_p()(n / n_total),
      obs = case_when(
        n == 0 ~ "aucune observation",
        n == 1 ~ glue("{n} observation"),
        .default = glue("{n} observations ({p})")
      )
    )

  glue(
    "{n_total} observations, {na$obs} contenant a minima une donn\u00e9es manquante"
  )
}


#' Recode dichotomous variables to a single 0/1 indicator
#'
#' Converts every two-level factor (except those excluded) to a 0/1 numeric
#' indicator, so gtsummary can display each on a single row.
#'
#' @param data A data frame to recode.
#' @param exclude Column name(s) to leave untouched; defaults to the first
#'   column.
#' @return The data frame with its dichotomous columns recoded to 0/1.
#' @export
#'
#' @examples
#' # `sex` (two-level factor) is recoded to a 0/1 indicator
#' show_single_row(datasets::penguins)
#'
show_single_row <- \(
  data,
  exclude = names(data[, 1])
) {
  all_dichotomous <- expr(c(where(~ nlevels(.) == 2), -all_of(exclude)))

  data |>
    mutate(across(!!all_dichotomous, ~ if_else(as.numeric(.) == 1, 0, 1)))
}


#' Display a reference marker on reference rows
#'
#' Places a symbol in the estimate and confidence-interval columns of the
#' reference rows via [gtsummary::modify_missing_symbol()]. The numeric estimate
#' column is left unchanged.
#'
#' @param data A `gtsummary` regression table object.
#' @param label Symbol to display on the reference rows.
#' @returns A `gtsummary` table with the reference marker displayed.
#' @export
#'
#' @examples
#' set.seed(1)
#' d <- data.frame(
#'   y = factor(rep(c("A", "B"), length.out = 60)),
#'   grp = factor(rep(c("g1", "g2", "g3"), length.out = 60))
#' )
#' fit <- glm(y ~ grp, binomial, d)
#' tbl <-
#'   gtsummary::tbl_regression(fit) |>
#'   add_ref_label()
#'
add_ref_label <- \(data, label = "Reference") {
  modify_missing_symbol(
    x = data,
    symbol = label,
    columns = c(estimate, conf.low, conf.high),
    rows = reference_row
  )
}
