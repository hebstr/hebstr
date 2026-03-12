#' Title
#'
#' @param x arg
#' @param filename arg
#' @param dir arg
#' @param suffix arg
#' @param sep arg
#' @param width arg
#' @param size arg
#' @param px arg
#' @param quiet arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_out <- \(
  x,
  filename = enexpr(x),
  dir = "output",
  suffix = "",
  sep = ".",
  width = NA,
  size = NULL,
  px = 2000,
  quiet = FALSE
) {

  clear_vars()

  cli_h1("easy_out")
  cat_line()

  cli_alert_info("Objet : {.strong {filename}} {.cls {class(x)}}")
  cat_line()

  if (!(is_ggplot(x) || inherits(x, c("ggmatrix", "gt_tbl", "gtsummary")))) {

    cli_abort(c(
      "{.strong {filename}} must be a gt/gtsummary object or a ggplot object",
      "i" = "Received object of class: {.cls {class(x)}}"
    ))

  }

  if (nzchar(suffix)) filename <- str_glue("{filename}{sep}{suffix}")

  fs::dir_create(path = dir)

  if (quiet) withr::local_options(easy_out.quiet = quiet)

  path <- fs::path(dir, filename)

  to_html <- fs::path(path, ext = "html")
  to_svg <- fs::path(path, ext = "svg")
  to_png <- fs::path(path, ext = "png")

### TAB -------------------------------------------------------------------------

  if (inherits(x, c("gt_tbl", "gtsummary"))) {

    if (inherits(x, "gtsummary")) x <- as_gt(x)

    if (R.version$os == "linux-gnu") Sys.setenv(OPENSSL_CONF = "/dev/null")

    if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

    width <-
    x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      str_extract("\\d+") |>
      as.numeric()

    cli_progress_step("Cr\u00e9ation du fichier au format HTML")

    gtsave(x, filename = to_html)

    cli_progress_step("Cr\u00e9ation du fichier au format PNG")

    webshot(
      url = to_html,
      file = to_png,
      vwidth = width + 0.1 * width,
      vheight = 1,
      zoom = 3
    )

    cli_progress_done()

    cli_path <- cli::col_br_red(str_glue("{path}.<html/png>"))

    cat_line()
    cli_alert_info("R\u00e9pertoire racine : {.path {here::here()}}")
    cli_alert_info("Fichiers enregistr\u00e9s dans {cli_path}")
    cat_line()

    cli_rule()

    if (!isTRUE(getOption("easy_out.quiet"))) browseURL(to_html)

### PLOT -------------------------------------------------------------------------

  } else if (is_ggplot(x) || inherits(x, "ggmatrix")) {

    cli_progress_step("Cr\u00e9ation du fichier au format SVG")

    .plot <- capturePlot(
      expr = x,
      filename = to_svg,
      grDevices::svg,
      height = size[1],
      width = size[2]
    )

    cli_progress_step("Cr\u00e9ation du fichier au format PNG")

    to_svg |>
      image_read_svg(height = px) |>
      image_write(to_png, format = "png")

    cli_progress_done()

    cli_path <- cli::col_br_red(str_glue("{path}.<svg/png>"))

    cat_line()
    cli_alert_info("R\u00e9pertoire parent : {.path {here::here()}}")
    cli_alert_info("Fichiers enregistr\u00e9s dans {cli_path}")
    cat_line()

    cli_rule()

    if (!isTRUE(getOption("easy_out.quiet"))) browseURL(.plot)

  }

}

#' Title
#'
#' @param x arg
#' @param filename arg
#' @param sep arg
#' @param ... arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_out_map <- \(
  x,
  filename = NULL,
  sep = ".",
  ...
) {

  if (is.null(filename)) filename <- enexpr(x)

  if (!is.list(x) || is.data.frame(x)) {

    cli_abort(c(
      "{.strong {filename}} must be a list of tables/figures",
      "i" = "Received object of class: {.cls {class(x)}}"
    ))

  }

  map_fun <- \(data, name) easy_out(
    x = data,
    filename = str_glue("{filename}{sep}{name}"),
    ...
  )

  iwalk(x, map_fun)

}
