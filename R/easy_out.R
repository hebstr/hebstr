#' Save a ggplot or gt table to disk
#'
#' Export a ggplot, gt, or gtsummary object to PNG (and SVG or HTML
#' depending on the object type). Opens the result in a browser unless
#' `quiet = TRUE`.
#'
#' @param x A ggplot, ggmatrix, gt_tbl, or gtsummary object.
#' @param filename Output filename (without extension). Defaults to the
#'   unevaluated expression passed as `x`.
#' @param dir Output directory. Created if it does not exist. Defaults
#'   to `getOption("easy_out.dir", "output")`.
#' @param suffix Optional suffix appended to `filename`.
#' @param sep Separator between `filename` and `suffix`.
#' @param width Width of the output. For tables: viewport width in pixels
#'   (default 700). For plots: SVG width in inches (default 7).
#' @param height Height in inches for SVG output of plots only. `NULL`
#'   (default) uses the nombre d'or: `width / 1.618`. Ignored for tables.
#' @param px Height in pixels for the PNG rasterization of plots.
#' @param quiet If `TRUE`, suppress auto-opening the output in a browser. Defaults
#'   to `getOption("easy_out.quiet", FALSE)`.
#'
#' @return `NULL` (invisibly). Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' easy_out(my_plot)
#' easy_out(my_table, suffix = "v2", quiet = TRUE)
#' }
#'
easy_out <- \(
  x,
  filename = enexpr(x),
  dir = getOption("easy_out.dir", default = "output"),
  suffix = "",
  sep = "_",
  width = NULL,
  height = NULL,
  px = 2000,
  quiet = getOption("easy_out.quiet", default = FALSE)
) {
  clear_vars()

  if (!is_bool(quiet)) {
    cli_abort("{.arg quiet} must be {.code TRUE} or {.code FALSE}.")
  }

  cli_h1("easy_out")
  cat_line()

  cli_alert_info("Object: {.strong {filename}} {.cls {class(x)}}")
  cat_line()

  if (!(is_ggplot(x) || inherits(x, c("ggmatrix", "gt_tbl", "gtsummary")))) {
    cli_abort(c(
      "{.strong {filename}} must be a gt/gtsummary object or a ggplot object",
      "i" = "Received object of class: {.cls {class(x)}}"
    ))
  }

  if (nzchar(suffix)) {
    filename <- paste0(filename, sep, suffix)
  }

  fs::dir_create(path = dir)

  path <- fs::path(dir, filename)
  to_png <- fs::path(path, ext = "png")

  cli_output <- \(files, browse) {
    files_list <- map_chr(
      files,
      ~ format_inline("{cli::col_br_red(fs::path_file(.))}")
    )

    cat_line()
    cli_alert_info("Files saved in {.strong {.path {here::here(dir)}}}")
    cli_ul(files_list)
    cat_line()

    cli_rule()

    if (!quiet) {
      browseURL(browse)

      test <- format_inline(
        "{.arg quiet = TRUE} or {.code options(easy_out.quiet = TRUE)}"
      )

      cli_inform(
        message = c(
          "i" = cli::col_grey('Set {test} to disable auto-opening in browser.')
        ),
        .frequency = "once",
        .frequency_id = "easy_out_quiet_hint"
      )
    }
  }

  ### TAB -------------------------------------------------------------------------

  if (inherits(x, c("gt_tbl", "gtsummary"))) {
    to_html <- fs::path(path, ext = "html")

    if (inherits(x, "gtsummary")) {
      x <- as_gt(x)
    }

    if (is.null(width)) {
      width <- 700
    }

    gt_width <-
      x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      str_extract("\\d+") |>
      as.numeric()

    if (length(gt_width) > 0 && !is.na(gt_width)) {
      width <- gt_width
    } else {
      x <- x |> tab_options(table.width = px(width))
    }

    cli_progress_step("Creating HTML file")

    gtsave(x, filename = to_html)

    cli_progress_step("Creating PNG file")

    webshot(
      url = to_html,
      file = to_png,
      vwidth = width * 1.1,
      vheight = 1,
      zoom = 3,
      quiet = TRUE
    )

    cli_progress_done()

    cli_output(
      files = c(to_html, to_png),
      browse = to_html
    )

    ### PLOT -------------------------------------------------------------------------
  } else if (is_ggplot(x) || inherits(x, "ggmatrix")) {
    to_svg <- fs::path(path, ext = "svg")

    if (is.null(width)) {
      width <- 7
    }
    if (is.null(height)) {
      height <- width / 1.618
    }

    cli_progress_step("Creating SVG file")

    capturePlot(
      expr = x,
      filename = to_svg,
      grDevices::svg,
      width = width,
      height = height
    )

    cli_progress_step("Creating PNG file")

    to_svg |>
      image_read_svg(height = px) |>
      image_write(to_png, format = "png")

    cli_progress_done()

    cli_output(
      files = c(to_svg, to_png),
      browse = to_svg
    )
  }

  invisible(NULL)
}

#' Save a list of ggplot or gt objects to disk
#'
#' Iterate over a named list and call [easy_out()] on each element, appending
#' the list name to the filename.
#'
#' @param x A named list of ggplot, ggmatrix, gt_tbl, or gtsummary objects.
#' @param filename Base filename. Defaults to the unevaluated expression
#'   passed as `x`.
#' @param sep Separator between `filename` and the list element name.
#' @param ... Additional arguments passed to [easy_out()].
#'
#' @return `NULL` (invisibly, via [easy_out()]).
#' @export
#'
#' @examples
#' \dontrun{
#' easy_out_map(list(fig1 = p1, fig2 = p2))
#' }
#'
easy_out_map <- \(
  x,
  filename = NULL,
  sep = "_",
  ...
) {
  if (is.null(filename)) {
    filename <- enexpr(x)
  }

  if (!is.list(x) || is.data.frame(x)) {
    cli_abort(c(
      "{.strong {filename}} must be a list of tables/figures",
      "i" = "Received object of class: {.cls {class(x)}}"
    ))
  }

  if (!is_named(x)) {
    cli_abort("{.arg x} must be a named list.")
  }

  map_fun <- \(data, name) {
    easy_out(
      x = data,
      filename = paste0(filename, sep, name),
      ...
    )
  }

  iwalk(x, map_fun)
}
