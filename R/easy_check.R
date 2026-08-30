#' Collect data-quality flags into a long report
#'
#' Evaluates a set of logical checks over a data frame and returns one row per
#' failing check, identified by `.id` and accompanied by whatever context
#' `.with` carries. The result is meant to be handed to a data manager, one
#' line per thing to look at, and reads well as a spreadsheet through
#' [get_xlsx()] and [easy_out()].
#'
#' @param .data A data frame to check.
#' @param ... Named logical expressions, one per check, evaluated in `.data`.
#'   A check is `TRUE` when it flags the row. Every expression must be named,
#'   and must evaluate to a logical vector; compute intermediate values in
#'   `.with` instead.
#' @param .id Name of the column identifying a row, as a string. Its values
#'   need not be unique: a repeated identifier is itself a legitimate thing to
#'   check for.
#' @param .with A call to [base::list()] holding the columns to carry alongside
#'   each flag, in the order they should appear. Name what is computed
#'   (`n_cures = induc_nb + adj_nb`) and give bare what already exists
#'   (`centre`). The columns are added to `.data` before the checks run, so a
#'   check can reference them. A computed element left unnamed, or a bare
#'   element that is not an existing column, is an error.
#' @param .name Name of the column holding the check names.
#' @param .na What to do with a check that evaluates to `NA`, having read a
#'   missing value and so decided nothing. `"drop"` leaves it out, as a passing
#'   check would be. `"flag"` reports it and adds a `status` column telling
#'   `fail` from `unknown`. A check is made total at its own site, as in
#'   `!is.na(a) & !is.na(b) & a < b`, which is where the knowledge of whether
#'   an absence is legitimate lives.
#'
#' @return A tibble with one row per flagged check: `.id`, `.name`, the
#'   `status` column under `.na = "flag"`, then the columns carried by `.with`.
#'   Rows follow the order of `.data`, and within a row the order the checks
#'   were declared.
#' @export
#'
#' @examples
#' df <- data.frame(
#'   id = 1:4,
#'   site = c("A", "B", "A", "B"),
#'   start = as.Date(c("2024-01-01", NA, "2024-03-01", "2024-04-01")),
#'   end = as.Date(c("2023-12-01", "2024-02-01", "2024-04-01", NA)),
#'   n_first = c(0, 3, 0, 2),
#'   n_second = c(1, 0, 0, 2)
#' )
#'
#' easy_check(
#'   df,
#'   start_missing = is.na(start),
#'   end_before_start = end < start,
#'   no_treatment = n_total == 0,
#'   .id = "id",
#'   .with = list(site, n_total = n_first + n_second)
#' )
#'
#' easy_check(
#'   df,
#'   end_before_start = end < start,
#'   .id = "id",
#'   .na = "flag"
#' )
#'
easy_check <- \(
  .data,
  ...,
  .id = "rowname",
  .with = NULL,
  .name = "check",
  .na = "drop"
) {
  .check_guards(.data, .id, .name, .na)

  with <- .check_with(.data, enquo(.with))
  .data <- with$data
  carried <- setdiff(with$declared, .id)

  if (.name %in% c(.id, carried)) {
    name <- .name

    cli_abort(
      "{.arg .name} must not be a carried column; {.val {name}} is one."
    )
  }

  flags <- .check_flags(.data, enquos(...), .id, .name, c("status", carried))

  .data |>
    ungroup() |>
    select(all_of(c(.id, carried))) |>
    bind_cols(flags) |>
    pivot_longer(
      all_of(names(flags)),
      names_to = .name,
      values_to = "status"
    ) |>
    .check_long(.na) |>
    relocate(all_of(.name), any_of("status"), .after = all_of(.id))
}

.check_guards <- \(.data, .id, .name, .na) {
  if (!is_string(.id)) {
    cli_abort("{.arg .id} must be a single column name, given as a string.")
  }

  if (!.id %in% names(.data)) {
    id <- .id

    cli_abort(c(
      "{.arg .id} must name a column of {.arg .data}; {.val {id}} is not one.",
      i = "{.fun tibble::rownames_to_column} adds one when the frame has none."
    ))
  }

  if (!is_string(.name)) {
    cli_abort("{.arg .name} must be a single column name, given as a string.")
  }

  if (!is_string(.na) || !.na %in% c("drop", "flag")) {
    cli_abort('{.arg .na} must be "drop" or "flag".')
  }
}

.check_with <- \(.data, quo) {
  if (quo_is_null(quo)) {
    return(list(data = .data, declared = character()))
  }

  expr <- quo_get_expr(quo)

  if (!is_call(expr, "list")) {
    cli_abort("{.arg .with} must be a call to {.fun list}.")
  }

  parts <- as.list(expr)[-1]

  if (!length(parts)) {
    cli_abort("{.arg .with} must hold at least one column.")
  }

  labels <- names2(parts)
  bare <- !nzchar(labels)
  unnamed <- bare & !map_lgl(parts, is_symbol)

  if (any(unnamed)) {
    at <- which(unnamed)

    cli_abort(c(
      "Every computed element of {.arg .with} must be named.",
      x = "Positions without a name: {at}.",
      i = "An existing column is carried by naming it bare, as in
           {.code .with = list(centre, groupe)}."
    ))
  }

  labels[bare] <- map_chr(parts[bare], as_string)
  absent <- setdiff(labels[bare], names(.data))

  if (length(absent)) {
    cli_abort(c(
      "A column carried by {.arg .with} must exist in {.arg .data}.",
      x = "Not a column of {.arg .data}: {.field {absent}}.",
      i = "Compute it instead, by giving it a name."
    ))
  }

  names(parts) <- labels

  list(data = mutate(.data, !!!parts), declared = labels)
}

.check_flags <- \(.data, quos, .id, .name, reserved) {
  if (!length(quos)) {
    cli_abort("{.arg ...} must hold at least one check.")
  }

  if (!is_named(quos)) {
    cli_abort("Every check must be named.")
  }

  groups <- group_vars(.data)
  taken <- intersect(names(quos), c(.id, .name, reserved, groups))

  if (length(taken)) {
    cli_abort("A check cannot be named {.field {taken}}.")
  }

  flags <- .data |>
    transmute(!!!quos) |>
    ungroup() |>
    select(-any_of(groups))

  wrong <- names(flags)[!map_lgl(flags, is.logical)]

  if (length(wrong)) {
    cli_abort(c(
      "Every check must evaluate to a logical vector.",
      x = "{.field {wrong}} {?does/do} not.",
      i = "Declare intermediate values in {.arg .with} rather than as checks."
    ))
  }

  flags
}

.check_long <- \(long, .na) {
  if (.na == "drop") {
    long |>
      filter(coalesce(status, FALSE)) |>
      select(-status)
  } else {
    long |>
      filter(coalesce(status, TRUE)) |>
      mutate(status = if_else(is.na(status), "unknown", "fail"))
  }
}
