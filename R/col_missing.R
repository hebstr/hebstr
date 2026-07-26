#' Collapse the missing-value rows into a single column
#'
#' Extracts the automatic `gtsummary` "missing" rows (one per variable) and
#' folds their per-group counts into a single `dm` column placed on the label
#' row, then drops the original missing rows. The counts are joined with `/`
#' across the grouping columns (or the overall column when the table is
#' ungrouped).
#'
#' The function is idempotent: a table that already carries a `dm` column is
#' returned unchanged with a warning, so the manual call and the
#' [tbl_format()] `missing` switch never stack. A table with no missing rows is
#' likewise returned unchanged with a warning.
#'
#' @param x A `gtsummary` table, typically built with
#'   [gtsummary::tbl_summary()].
#' @param prefix Character string prepended to each collapsed count (e.g.
#'   `"n ="`), separated from it by a space. Defaults to `""`, which leaves the
#'   count on its own.
#' @param empty Replacement for a missing per-group count. Defaults to `"0"`.
#' @param header Column header for the `dm` column, interpreted as markdown.
#'   Defaults to the `labs$col_missing` option wrapped in bold (`MD` in English,
#'   `DM` in French).
#' @param align Column alignment for the `dm` column. Defaults to `"center"`.
#'
#' @returns The `gtsummary` table with the missing rows collapsed into a `dm`
#'   column, or the input unchanged (with a warning) when a `dm` column already
#'   exists or no missing rows are present.
#'
#' @examples
#' set_opts()
#'
#' df <- data.frame(
#'   grp = c(rep("a", 10), rep("b", 10)),
#'   age = c(seq_len(18), NA, NA)
#' )
#' tbl <- gtsummary::tbl_summary(df, by = grp, include = age)
#' col_missing(tbl)
#'
#' @export
#'
col_missing <- \(
  x,
  prefix = "",
  empty = "0",
  header = str_glue("**{check_opts(labs$col_missing)}**"),
  align = "center"
) {
  if (!inherits(x, "gtsummary")) {
    cli_abort(
      c(
        "{.arg x} must be a {.cls gtsummary} table.",
        i = "Build it with {.fun gtsummary::tbl_summary} before {.fun col_missing}."
      )
    )
  }

  if ("dm" %in% names(x$table_body)) {
    cli_warn(
      c(
        "The table already carries a {.field dm} column.",
        i = "{.fun col_missing} is idempotent; the table is returned unchanged."
      )
    )
    return(x)
  }

  if (!any(x$table_body$row_type %in% "missing")) {
    cli_warn(
      c(
        "No missing rows to collapse.",
        i = "The table is returned unchanged."
      )
    )
    return(x)
  }

  all_stat <- x$table_body |> select(matches("^stat_\\d+$")) |> names()
  group_cols <- setdiff(all_stat, "stat_0")
  stat_cols <- if (length(group_cols) > 0) group_cols else all_stat
  first_stat <- all_stat[1]
  prefix_sep <- if (nzchar(prefix)) " " else ""

  x |>
    modify_table_body(\(body) {
      dm <- body |>
        filter(row_type == "missing") |>
        mutate(across(all_of(stat_cols), \(s) replace_na(s, empty))) |>
        rowwise() |>
        mutate(
          dm = str_glue(
            "{prefix}{prefix_sep}{str_c(c_across(all_of(stat_cols)), collapse = '/')}"
          )
        ) |>
        ungroup() |>
        select(variable, dm)
      body |>
        left_join(dm, by = "variable") |>
        mutate(
          dm = if_else(row_type == "label", as.character(dm), NA_character_)
        ) |>
        relocate(dm, .before = all_of(first_stat)) |>
        filter(!row_type %in% "missing")
    }) |>
    modify_header(dm = header) |>
    modify_column_alignment(columns = dm, align = align)
}
