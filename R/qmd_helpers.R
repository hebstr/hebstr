#' Create a styled gt table for Quarto documents
#'
#' Converts a data frame or gtsummary object into a [gt::gt] table with
#' custom font and bold column labels. Designed for use in Quarto documents.
#'
#' @param data A `data.frame` or `gtsummary` object.
#' @param top_n If not `NULL`, a positive integer passed to [gt::gt_preview()]
#'   to display only the first `top_n` rows. Ignored on a `gtsummary` input,
#'   which is converted whole.
#' @param font_family Font family for the table's text. When [set_opts()] has
#'   been called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param font_size Font size in pixels. Defaults to `15`.
#' @param id HTML id attribute for the table. Defaults to `NULL`, which lets
#'   [gt::gt()] generate a random unique id. This id scopes gt's own stylesheet,
#'   so a fixed default makes every table in a document share one scope, where
#'   the last stylesheet wins for all of them. A readable, stable anchor belongs
#'   on the Quarto crossref label, not here. Has no effect on the `top_n`
#'   preview of a data frame: [gt::gt_preview()] builds the table itself and
#'   exposes no id argument.
#' @param ... Additional arguments passed to [gt::tab_options()].
#'
#' @returns A [gt::gt] object.
#' @export
#'
#' @examples
#' gt_qmd(mtcars)
#' gt_qmd(mtcars, top_n = 2)
#'
gt_qmd <- \(
  data,
  top_n = NULL,
  font_family = .text_font(),
  font_size = 15,
  id = NULL,
  ...
) {
  if (!inherits(data, "data.frame") && !inherits(data, "gtsummary")) {
    cli_abort(c(
      "{.arg data} must be a {.cls data.frame} or {.cls gtsummary} object",
      "x" = "Object of class {.cls {class(data)}} supplied"
    ))
  }

  if (!is.null(top_n)) {
    if (!is.numeric(top_n) || length(top_n) != 1 || top_n < 1) {
      cli_abort("{.arg top_n} must be a single positive numeric")
    }
  }

  data <- if (inherits(data, "gtsummary")) {
    as_gt(data, id = id)
  } else if (is.null(top_n)) {
    gt(data, id = id)
  } else {
    gt_preview(data, top_n = top_n)
  }

  data |>
    tab_options(
      table.font.names = .font_stack(font_family),
      table.font.size = px(font_size),
      column_labels.border.top.color = "white",
      ...
    ) |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_column_labels()
    )
}

#' Glue strings with Quarto-safe delimiters
#'
#' Wrapper around [glue::glue()] using `<<` and `>>` as delimiters instead of
#' `{` and `}`, to avoid conflicts with Quarto/R Markdown curly brace syntax.
#' Expressions are evaluated in the caller's environment.
#'
#' @param string A single character string with `<<expr>>` placeholders.
#'
#' @returns A [glue::glue()] string.
#' @export
#'
#' @examples
#' name <- "world"
#' glue_qmd("Hello <<name>>")
#' glue_qmd("```{r, echo=TRUE}\n<<name>>\n```")
#'
glue_qmd <- \(string) {
  if (!is.character(string) || length(string) != 1) {
    cli_abort(c(
      "{.arg string} must be a single character string",
      "x" = "Object {.cls {class(string)}} of length {length(string)} supplied"
    ))
  }
  glue(string, .open = "<<", .close = ">>", .envir = parent.frame())
}

#' Include an external code file in a Quarto document
#'
#' Generates a Quarto fenced code block that includes the contents of an
#' external source file with line numbers enabled.
#'
#' @param src Path to the source file to include (relative to the Quarto
#'   project root).
#' @param lang Language identifier for syntax highlighting. Defaults to `"r"`.
#'
#' @returns A [glue::glue()] string containing the Quarto include directive.
#' @export
#'
#' @examples
#' include_code_file("script.R")
#' include_code_file("query.sql", lang = "sql")
#'
include_code_file <- \(src, lang = "r") {
  if (!is.character(src) || length(src) != 1) {
    cli_abort(c(
      "{.arg src} must be a single character string",
      "x" = "Object of class {.cls {class(src)}} of length {length(src)} supplied"
    ))
  }

  if (!is.character(lang) || length(lang) != 1) {
    cli_abort(c(
      "{.arg lang} must be a single character string",
      "x" = "Object of class {.cls {class(lang)}} of length {length(lang)} supplied"
    ))
  }

  glue_qmd(
    "
  ```{.<<lang>> include='<<src>>' code-line-numbers='true'}
  ```
  "
  )
}

#' Publish an easy_out() output in a rendered document
#'
#' Hands the document either the live object or the artefact [easy_out()]
#' wrote for it, whichever the render target can carry. The object is passed
#' by name, and the path of its artefact is derived with the rules
#' [easy_out()] used to write it, so a document never restates a filename and
#' the two sides cannot drift apart.
#'
#' @param x A supported object, or one element of a named list of them, as
#'   `x$element`. Passed by name: the path is derived from it.
#' @param filename Base name the output was written under, when [easy_out()]
#'   was given one. Defaults to `NULL`, which derives it from `x`.
#' @param dir,subdir,suffix,sep The path-forming arguments of [easy_out()],
#'   with the same defaults and the same meaning. Pass here whatever the
#'   writing call passed.
#'
#' @details
#' What is handed over follows the object's class and the render target:
#'
#' | Class | HTML | Any other target |
#' | --- | --- | --- |
#' | `gt_tbl`, `gtsummary` | the object | the PNG |
#' | `flextable` | the object | the object |
#' | `htmlwidget` | the object | the PNG |
#' | `hebstr_dict` | the widget | the PNG |
#' | ggplot, `ggmatrix`, grid grob | the SVG | the PNG |
#'
#' The class carries the format decision the document made upstream: under
#' `options(hebstr.docx = TRUE)` a table is a `flextable` and goes to Word
#' whole, whereas a document leaving the option unset gets a `gt_tbl` and its
#' PNG. One reader serves both.
#'
#' A figure is always the file its script wrote, never a redraw. Left to
#' knitr, it would be drawn on the chunk device, at other dimensions and with
#' other fonts, and no error would say so.
#'
#' A bare list is a multi-panel figure and nothing else, published as one
#' figure holding every panel. Reading a table or a widget out of a list is
#' refused rather than allowed: it would work under Word, where every panel is
#' a path, and break under HTML, where a list of live objects prints as a
#' list.
#'
#' A widget has a PNG only when [easy_out()] was called with `png = TRUE`; the
#' missing file aborts the render rather than dropping the output.
#'
#' @returns The object itself, or the artefact wrapped by
#'   [knitr::include_graphics()].
#' @export
#'
#' @examples
#' \dontrun{
#' out_qmd(tbl_pop)
#' out_qmd(tbl_codes$cim10)
#' out_qmd(fig_paths)
#' }
#'
out_qmd <- \(
  x,
  filename = NULL,
  dir = getOption("easy_out.dir", default = "output"),
  subdir = TRUE,
  suffix = "",
  sep = "_"
) {
  check_installed("knitr", reason = "to publish an output in a document.")

  if (
    !is.null(filename) && (!is_scalar_character(filename) || is.na(filename))
  ) {
    cli_abort("{.arg filename} must be a single string.")
  }

  .check_subdir(subdir)

  check_affix <- \(value, arg) {
    if (length(value) != 1L || is.na(value)) {
      cli_abort("{.arg {arg}} must be a single non-missing value.")
    }
  }

  check_affix(suffix, "suffix")
  check_affix(sep, "sep")

  # captured before the guards force x, after which enexpr() hands back the value
  x_expr <- enexpr(x)
  label <- as_label(x_expr)

  ref <- .qmd_ref(x_expr, filename)

  panels <- .qmd_panels(x, label)
  kind <- if (is.null(panels)) .qmd_check(x, label) else "figure"

  # the class already carries the format decision made upstream, so the render
  # target is read from knitr rather than from .is_docx(): a document that
  # publishes PNGs never sets hebstr.docx, and the option would answer for it
  html <- knitr::is_html_output()

  live <- switch(
    kind,
    flextable = TRUE,
    table = html,
    widget = html,
    dict = html,
    figure = FALSE
  )

  if (live) {
    return(if (kind == "dict") x$output else x)
  }

  ext <- if (kind == "figure" && html) "svg" else "png"

  knitr::include_graphics(.qmd_path(ref, panels, ext, dir, subdir, suffix, sep))
}

# a symbol names a whole output and symbol$symbol one element of a bare list,
# the form easy_out() refuses for itself: the writer takes the list and the
# reader one of the files it wrote. Any other expression is refused even under
# an explicit filename, an element the shape does not expose being lost silently
.qmd_ref <- \(expr, filename) {
  base <- if (is_symbol(expr)) as_string(expr) else NULL
  elem <- NULL

  if (is_call(expr, "$")) {
    parts <- as.list(expr)[-1]

    if (all(map_lgl(parts, is_symbol))) {
      base <- as_string(parts[[1]])
      elem <- as_string(parts[[2]])
    }
  }

  if (is.null(base)) {
    cli_abort(c(
      "{.arg x} must be a named object, followed by at most one {.code $element}.",
      "x" = "{.arg x} was given as {.code {as_label(expr)}}.",
      "i" = "Assign the object first: its output is named after it."
    ))
  }

  list(base = filename %||% base, elem = elem)
}

.qmd_kind <- \(x) {
  if (is_ggplot(x) || inherits(x, "ggmatrix") || grid::is.grob(x)) {
    return("figure")
  }
  if (inherits(x, c("gt_tbl", "gtsummary"))) {
    return("table")
  }
  if (inherits(x, "flextable")) {
    return("flextable")
  }
  if (inherits(x, "hebstr_dict")) {
    return("dict")
  }
  if (inherits(x, "htmlwidget")) {
    return("widget")
  }

  NULL
}

# a workbook is the one class easy_out() writes that no document can carry,
# so it is named apart rather than folded into the unsupported message
.qmd_check <- \(x, label) {
  if (inherits(x, "wbWorkbook")) {
    cli_abort(c(
      "{.strong {label}} is a workbook, which no rendered document can hold.",
      "i" = "{.fun easy_out} writes it as an {.field xlsx} file, to be linked rather than published."
    ))
  }

  kind <- .qmd_kind(x)

  if (is.null(kind)) {
    cli_abort(c(
      "{.strong {label}} must be a gt/gtsummary/flextable, ggplot, grid grob, or widget object, or a named list of figures",
      "i" = "Received object of class: {.cls {class(x)}}"
    ))
  }

  kind
}

# figures only: include_graphics() vectorises over paths, where a list of live
# objects prints as a list. Accepting tables here would publish under Word and
# break under HTML, the one asymmetry a two-format document cannot afford
.qmd_panels <- \(x, label) {
  if (!is_bare_list(x)) {
    return(NULL)
  }

  if (!length(x) || !is_named(x)) {
    cli_abort(c(
      "{.strong {label}} must be a named, non-empty list.",
      "i" = "Its names are what {.fun easy_out} wrote each panel under."
    ))
  }

  wrong <- !map_lgl(x, ~ identical(.qmd_kind(.x), "figure"))

  if (any(wrong)) {
    first <- names(x)[wrong][1]

    cli_abort(c(
      "{.strong {label}} must hold figures only: a list is published as one multi-panel figure.",
      "x" = "{.val {first}} is of class: {.cls {class(x[[first]])}}",
      "i" = "Publish it on its own with {.code {label}${first}}."
    ))
  }

  names(x)
}

.qmd_path <- \(ref, panels, ext, dir, subdir, suffix, sep) {
  folder <- .out_subdir(ref$base, subdir)
  out_dir <- if (isFALSE(folder)) fs::path(dir) else fs::path(dir, folder)

  # the folder comes from the base alone, so the panels and the suffixed
  # variants of one output stay side by side
  stem <- str_flatten(c(ref$base, ref$elem), collapse = sep)
  stems <- if (is.null(panels)) stem else str_c(stem, sep, panels)

  if (nzchar(suffix)) {
    stems <- str_c(stems, sep, suffix)
  }

  fs::path(out_dir, map_chr(stems, .out_name), ext = ext)
}
