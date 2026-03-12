#' Source all scripts from a directory
#'
#' Executes all files found in a directory, excluding files whose name
#' starts with a given prefix.
#'
#' @param dir Path to the directory containing the scripts. Defaults to
#'   `"scripts"`.
#' @param except_starts_with Prefix used to exclude files. Files starting with
#'   this string are skipped. Defaults to `"_"`.
#' @param ext File extension to match. Defaults to `".R"`.
#'
#' @return Called for its side effects. Returns `NULL` invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' # source all R scripts in "scripts/", except those starting with "_"
#' auto_exec()
#'
#' # source all markdown files in "example/scripts/", except test_*.md
#' auto_exec(
#'   dir = "example/scripts",
#'   except_starts_with = "test_",
#'   ext = ".md"
#' )
#' }
#'
auto_exec <- \(
  dir = "scripts",
  except_starts_with = "_",
  ext = ".R"
) {

  cli_h1("auto_exec")
  cat_line()

  if (!fs::dir_exists(dir)) cli_abort(
    "Aucun r\u00e9pertoire nomm\u00e9 {.path {dir}} trouv\u00e9 dans {.path {here::here()}}"
  )

  files <-
  list.files(dir) |>
    discard(startsWith, except_starts_with) |>
    keep(endsWith, ext)

  if (length(files) == 0) cli_abort(c(
    "Aucun fichier {.code *{ext}} trouv\u00e9 dans {.path {dir}}",
    "i" = "Fichiers exclus : pr\u00e9fixe {.val {except_starts_with}}"
  ))

  cli_alert_info("R\u00e9pertoire : {.path {dir}}")
  cli_alert_info("Fichiers sourc\u00e9s : {.file {files}}")
  cat_line()

  walk(files, ~ source(fs::path(dir, .)))

  cli_rule()

}
