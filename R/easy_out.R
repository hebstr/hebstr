#' Title
#'
#' @param x
#' @param filename
#' @param dir
#' @param suffix
#' @param width
#' @param height
#' @param size
#' @param print
#'
#' @return
#' @export
#'
#' @examples
#'
easy_out <- \(x,
              filename = NULL,
              dir = "output",
              suffix = NULL,
              width = NA,
              height = NULL,
              size = NULL,
              print = NULL) {

  cli::cli_h1("easy_out")
  cli::cli_text("\n\n")

  if (R.version$os == "linux-gnu") Sys.setenv(OPENSSL_CONF = "/dev/null")

  if (!webshot::is_phantomjs_installed()) webshot::install_phantomjs()

  if (is.null(filename)) filename <- rlang::enexpr(x)

  if (!dir.exists(dir)) dir.create(path = dir, recursive = TRUE)

  if (!is.null(suffix)) filename <- glue::glue("{filename}_{suffix}")

  path <- glue::glue("{dir}/{filename}")
  to_html <- glue::glue("{path}.html")
  to_svg <- glue::glue("{path}.svg")
  to_png <- glue::glue("{path}.png")

### TAB -------------------------------------------------------------------------

  if (stringr::str_detect(class(x)[1], "tbl")) {

    width <-
    x[["_options"]] |>
      dplyr::filter(parameter == "table_width") |>
      dplyr::pull(value) |>
      unlist() |>
      stringr::str_extract("\\d+") |>
      as.numeric()

    if (class(x)[1] != "gt_tbl") {

      x <- gtsummary::as_gt(x)

    } else if (names(x$`_data`)[1] == "variable") {

      if (!stringr::str_detect(x$`_data`$var_label, "ref:")[1]) {

        vars <- "(variable|var_type|test_name)($|_1)"

        print(x$`_data` |>
              dplyr::select(dplyr::matches(vars)) |>
              dplyr::rename_with(~ stringr::str_remove(., "_1")) |>
              dplyr::relocate(var_type, .after = variable) |>
              dplyr::distinct())

      } else if (!is.null(print)) print(print)

    }

    cli::cli_text("\n\n")
    cli::cli_progress_step("Creating HTML file")

    gt::gtsave(x, file = to_html)

    utils::browseURL(to_html)

    cli::cli_progress_step("Capturing HTML file to PNG")

    webshot::webshot(to_html,
                     file = to_png,
                     vwidth = width + width / 10,
                     vheight = 1,
                     zoom = 3)

    cli::cli_progress_done()

### PLOT -------------------------------------------------------------------------

  } else if (ggplot2::is.ggplot(x)) {

    cli::cli_progress_step("Creating SVG file")

    htmltools::capturePlot(x,
                           to_svg,
                           grDevices::svg,
                           height = size[1],
                           width = size[2]) |>
      utils::browseURL()

    cli::cli_progress_step("Capturing SVG file to PNG")

    ggplot2::ggsave(to_png,
                    height = size[1],
                    width = size[2],
                    dpi = 500)

    cli::cli_progress_done()

  }

  if (!is.null(print)) print(print)

### CLI --------------------------------------------------------------------------

  cli::cli_text("\n\n")
  cli::cli_alert_info("{.strong Destination}")
  cli::cli_ul()
    cli::cli_li("Working directory: {.path {getwd()}}")
    cli::cli_li("Files: {cli::col_br_red(path)}")
    cli::cli_end()
  cli::cli_text("\n\n")
  cli::cli_rule()

}


#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
easy_out_map <- \(data,
                  filename = NULL,
                  suffix = seq(data),
                  size = NULL) {

  if (is.null(filename)) filename <- rlang::enexpr(data)

  purrr::map2(.x = data,
              .y = suffix,
              ~ easy_out(.x,
                         filename = glue("{filename}.{.y}"),
                         size = size))

}
