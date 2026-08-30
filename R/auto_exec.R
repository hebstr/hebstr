#' Source all scripts from a directory
#'
#' Executes the files found in a directory, keeping those whose name matches
#' `include` and dropping those matching `exclude`.
#'
#' @param dir Path to the directory containing the scripts. Defaults to
#'   `"scripts"`.
#' @param include Regular expression a filename must match to be sourced.
#'   `NULL` (the default) keeps every file. Matched against the name alone,
#'   extension included, never against the path.
#' @param exclude Regular expression excluding a file whose name matches it.
#'   Defaults to `"^_"`, the package convention marking a script the sweep
#'   skips. `NULL` excludes nothing. Applied after `include`, so a file
#'   matching both is skipped: `include = "^tbl"` still leaves `_tbl_wip.R`
#'   out. Matched by [stringr::str_detect()], whose engine takes lookarounds,
#'   so an inclusion can also be written as a negation: `"^(?!tbl)"`.
#' @param ext File extension to match. Defaults to `".R"`.
#' @param quiet If `TRUE`, suppresses all cli output. Defaults to `FALSE`.
#'
#' @return Called for its side effects. Returns `NULL` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # source all R scripts in "scripts/", except those starting with "_"
#' auto_exec()
#'
#' # only the table scripts, the underscore convention still applying
#' auto_exec(include = "^tbl")
#'
#' # source all markdown files in "example/scripts/", except test_*.md
#' auto_exec(
#'   dir = "example/scripts",
#'   exclude = "^test_",
#'   ext = ".md"
#' )
#' }
auto_exec <- \(
  dir = "scripts",
  include = NULL,
  exclude = "^_",
  ext = ".R",
  quiet = FALSE
) {
  check_filter <- \(value, arg) {
    valid <- is.null(value) || (is_scalar_character(value) && !is.na(value))

    if (!valid) {
      cli_abort("{.arg {arg}} must be a single regular expression, or NULL.")
    }
  }

  check_filter(include, "include")
  check_filter(exclude, "exclude")

  if (!quiet) {
    cli_h1("auto_exec")
    cat_line()
  }

  if (!fs::dir_exists(dir)) {
    cli_abort(
      "No directory named {.path {dir}} found in {.path {here::here()}}."
    )
  }

  files <- list.files(dir)

  if (!is.null(include)) {
    files <- keep(files, str_detect, include)
  }

  if (!is.null(exclude)) {
    files <- discard(files, str_detect, exclude)
  }

  files <- keep(files, endsWith, ext)

  if (length(files) == 0) {
    cli_abort(c(
      "No {.code *{ext}} file found in {.path {dir}}.",
      if (!is.null(include)) c("i" = "Kept: names matching {.val {include}}."),
      if (!is.null(exclude)) {
        c("i" = "Excluded: names matching {.val {exclude}}.")
      }
    ))
  }

  if (!quiet) {
    cli_alert_info("Directory: {.path {dir}}")
    cli_alert_info("Sourced files: {.file {files}}")
    cat_line()
  }

  walk(
    files,
    \(f) {
      tryCatch(
        source(fs::path(dir, f)),
        error = \(e) {
          cli_abort(
            "Failed to execute script {.file {f}}.",
            parent = e
          )
        }
      )
    }
  )

  if (!quiet) {
    cli_rule()
  }

  invisible(NULL)
}
