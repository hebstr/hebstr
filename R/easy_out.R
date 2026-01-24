#' Title
#'
#' @param x arg
#' @param filename arg
#' @param dir arg
#' @param suffix arg
#' @param width arg
#' @param height arg
#' @param size arg
#' @param px arg
#' @param assign arg
#' @param .quiet arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_out <- \(x,
              filename = enexpr(x),
              dir = "output",
              suffix = NULL,
              width = NA,
              height = NULL,
              size = NULL,
              px = 2000,
              assign = TRUE,
              .quiet = if (exists("quiet")) quiet else FALSE) {

  clear_vars()

  cli_h1("easy_out")
  cli_text("\n\n")

  cli_progress_step("Creating {.strong {filename}}")

  if (!(inherits(x, "gg") || inherits(x, "gt_tbl") || inherits(x, "gtsummary"))) {

    cli_abort(
      c("{.strong {filename}} must be a gt/gtsummary object or a ggplot object",
        "i" = "Received object of class: {.cls {class(x)}}")
    )

  }

  if (!is.null(suffix)) filename <- glue("{filename}_", suffix)

  if (!dir.exists(dir)) dir.create(path = dir, recursive = TRUE)

  if (assign && !as.character(filename) %in% ls()) {

    assign(as.character(filename), x, envir = .GlobalEnv)

  }

  path <- glue("{dir}/{filename}")
  to_html <- glue("{path}.html")
  to_svg <- glue("{path}.svg")
  to_png <- glue("{path}.png")

### TAB -------------------------------------------------------------------------

  if (inherits(x, "gt_tbl") || inherits(x, "gtsummary")) {

    if (R.version$os == "linux-gnu") Sys.setenv(OPENSSL_CONF = "/dev/null")

    if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

    if (!"gt_tbl" %in% class(x)) x <- as_gt(x)

    width <-
    x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      unlist() |>
      str_extract("\\d+") |>
      as.numeric()

    cli_progress_step("Creating HTML file")

    gtsave(x, filename = to_html)

    if (!.quiet) browseURL(to_html)

    cli_progress_step("Capturing HTML to PNG")

    to_html |>
      webshot(file = to_png,
              vwidth = width + width / 10,
              vheight = 1,
              zoom = 3)

    cli_progress_done()

### PLOT -------------------------------------------------------------------------

  } else if (inherits(x, "gg")) {

    cli_progress_step("Creating SVG file")

    .plot <-
    capturePlot(expr = x,
                filename = to_svg,
                grDevices::svg,
                height = size[1],
                width = size[2])

    if (!.quiet) browseURL(.plot)

    cli_progress_step("Capturing SVG to PNG")

    to_svg |>
      image_read_svg(height = px) |>
      image_write(to_png, format = "png")

    cli_progress_done()

  }

### CLI --------------------------------------------------------------------------

  cli_text("\n\n")
  cli_alert_info("{.strong Destination}")
  cli_ul()
  cli_ul()
    cli_li("R\u00e9dpertoire : {.path {dir}}")
    cli_li("Filename : {cli::col_br_red(filename)}")
    cli_end()
  cli_text("\n\n")
  cli_rule()

}

#' Title
#'
#' @param data arg
#' @param filename arg
#' @param dir arg
#' @param sep arg
#' @param size arg
#'
#' @return arg
#' @export
#'
#' @examples "arg"
#'
easy_out_map <- \(data,
                  filename = NULL,
                  dir = "output",
                  sep = ".",
                  size = NULL) {

  if (is.null(filename)) filename <- enexpr(data)

  if (!is.list(data)) {

    cli_abort("{.strong {filename}} must be a list of tables or figures")

  }

  data |>
    imap(~ easy_out(x = .x,
                    filename = glue("{filename}{sep}{.y}"),
                    dir = dir,
                    size = size))

}
