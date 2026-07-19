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
#' @param id HTML id attribute for the table. Defaults to `"tbl-id"`. Has no
#'   effect on the `top_n` preview of a data frame: [gt::gt_preview()] builds
#'   the table itself and exposes no id argument.
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
  id = "tbl-id",
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
      table.font.names = font_family,
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
