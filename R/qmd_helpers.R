#' Create a styled gt table for Quarto documents
#'
#' Converts a data frame or gtsummary object into a [gt::gt] table with
#' custom font and bold column labels. Designed for use in Quarto documents.
#'
#' @param data A `data.frame` or `gtsummary` object.
#' @param top_n If not `NULL`, a positive integer passed to [gt::gt_preview()]
#'   to display only the first `top_n` rows.
#' @param font_family Font family name. Defaults to `"luciole"`.
#' @param font_size Font size in pixels. Defaults to `15`.
#' @param id HTML id attribute for the table. Defaults to `"tbl-id"`.
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
  font_family = "luciole",
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
    as_gt(data)
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

#' Title
#'
#' @param src arg
#' @param lang arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
include_code_file <- \(src, lang = "r") {
  glue_qmd(
    "
  ```{.<<lang>> include='<<src>>' code-line-numbers='true'}
  ```
  "
  )
}

#' Title
#'
#' @param src arg
#' @param name arg
#' @param suffix arg
#' @param sep arg
#' @param lang arg
#'
#' @returns arg
#' @export
#'
#' @examples "arg"
#'
add_code_file <- \(
  src,
  name = NULL,
  suffix = NULL,
  sep = " ",
  lang = NULL
) {
  name <- name %||% str_remove(src, ".+/")

  if (!is.null(suffix)) {
    name <- glue("{name}{sep}{suffix}")
  }

  lang <- lang %||% tolower(str_match(src, "\\w+$"))

  glue_qmd(
    "
  ::: {add-from=<<src>> code-line-numbers='true' code-filename=\"<<name>>\"}
  ```<<lang>>
  ```
  :::
  "
  )
}
