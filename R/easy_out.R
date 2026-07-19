#' Save a ggplot, gt table, or grid graphic to disk
#'
#' Export a ggplot, gt, gtsummary, or grid grob object to PNG (and SVG or
#' HTML depending on the object type). Opens the result in a browser unless
#' `quiet = TRUE`.
#'
#' @param x A ggplot, ggmatrix, gt_tbl, gtsummary, or grid grob object (for
#'   example a Gmisc flowchart built from `boxGrob()`/`connectGrob()`).
#' @param filename Output filename (without extension). Defaults to the
#'   unevaluated expression passed as `x`.
#' @param dir Output directory. Created if it does not exist. Defaults
#'   to `getOption("easy_out.dir", "output")`.
#' @param suffix Optional suffix appended to `filename`.
#' @param sep Separator between `filename` and `suffix`.
#' @param width Width of the output. For tables: viewport width in pixels
#'   (default 700). For plots and grid graphics: SVG width in inches
#'   (default 7). For a grid grob under `crop = TRUE`, `width` and `height`
#'   are a canvas budget rather than the size of the exported file, which
#'   is trimmed back to the drawing it contains.
#' @param height Height in inches for SVG output of plots and grid graphics
#'   only. `NULL` (default) uses the nombre d'or: `width / 1.618`. Ignored
#'   for tables.
#' @param px Height in pixels for the PNG rasterization of plots and grid
#'   graphics.
#' @param crop If `TRUE` (the default, read from
#'   `getOption("easy_out.crop")`), trim the SVG canvas of a grid grob to
#'   the bounding box of the drawing, keeping a small margin. Grob positions
#'   are relative to the whole page, so a drawing covering a sub-rectangle
#'   leaves an empty band that no `width`/`height` value removes. Ignored
#'   for tables and plots, whose margins come from the theme.
#' @param quiet If `TRUE`, suppress auto-opening the output in a browser. Defaults
#'   to `getOption("easy_out.quiet", FALSE)`.
#' @param export If `FALSE`, return without writing anything. Defaults to
#'   `getOption("easy_out.export")`, itself defaulting to `FALSE` under
#'   `options(hebstr.docx = TRUE)`: a Word run renders tables through
#'   [flextable::flextable()], leaving no object to export to HTML or PNG.
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
  filename = as_label(enexpr(x)),
  dir = getOption("easy_out.dir", default = "output"),
  suffix = "",
  sep = "_",
  width = NULL,
  height = NULL,
  px = 1200,
  crop = getOption("easy_out.crop", default = TRUE),
  quiet = getOption("easy_out.quiet", default = FALSE),
  export = getOption(
    "easy_out.export",
    default = !getOption("hebstr.docx", default = FALSE)
  )
) {
  if (!is_bool(quiet)) {
    cli_abort("{.arg quiet} must be {.code TRUE} or {.code FALSE}.")
  }

  if (!is_bool(export)) {
    cli_abort("{.arg export} must be {.code TRUE} or {.code FALSE}.")
  }

  if (!is_bool(crop)) {
    cli_abort("{.arg crop} must be {.code TRUE} or {.code FALSE}.")
  }

  if (!export) {
    return(invisible(NULL))
  }

  clear_vars()

  cli_h1("easy_out")
  cat_line()

  cli_alert_info("Object: {.strong {filename}} {.cls {class(x)}}")
  cat_line()

  is_supported <-
    is_ggplot(x) ||
    inherits(x, c("ggmatrix", "gt_tbl", "gtsummary")) ||
    grid::is.grob(x)

  if (!is_supported) {
    cli_abort(c(
      "{.strong {filename}} must be a gt/gtsummary, ggplot, or grid grob object",
      "i" = "Received object of class: {.cls {class(x)}}",
      if (inherits(x, "flextable")) {
        c(
          "i" = "{.fun tbl_format} returns a {.cls flextable} under {.code options(hebstr.docx = TRUE)}, for Word output. Exporting a {.cls flextable} is out of scope.",
          "i" = "To export, build the table without {.code hebstr.docx} so {.fun tbl_format} returns a {.cls gt_tbl}. To skip the export instead, leave {.arg export} at its {.code hebstr.docx} default."
        )
      }
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
    cli_alert_info("Files saved in {.strong {.path {fs::path_abs(dir)}}}")
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
      str_extract("^(\\d+)px$", group = 1) |>
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

    ggsave(
      filename = to_svg,
      plot = x,
      device = svglite::svglite,
      width = width,
      height = height
    )

    cli_progress_step("Creating PNG file")

    svg_to_png(to_svg, to_png, px)

    cli_progress_done()

    cli_output(
      files = c(to_svg, to_png),
      browse = to_svg
    )

    ### GROB -------------------------------------------------------------------------
  } else if (grid::is.grob(x)) {
    to_svg <- fs::path(path, ext = "svg")

    if (is.null(width)) {
      width <- 7
    }
    if (is.null(height)) {
      height <- width / 1.618
    }

    cli_progress_step("Creating SVG file")

    svglite::svglite(to_svg, width = width, height = height)
    grid::grid.newpage()
    grid::grid.draw(x)
    grDevices::dev.off()

    cli_progress_step("Creating PNG file")

    svg_to_png(to_svg, to_png, px, crop = crop)

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
    filename <- as_label(enexpr(x))
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

svg_to_png <- \(to_svg, to_png, px, crop = FALSE) {
  lines <- readLines(to_svg)

  if (!any(grepl("xml:space", lines, fixed = TRUE))) {
    lines <- sub("<svg ", '<svg xml:space="preserve" ', lines, fixed = TRUE)
    writeLines(lines, to_svg)
  }

  if (crop) {
    svg_crop(to_svg)
  }

  to_svg |>
    image_read_svg(height = px) |>
    image_write(to_png, format = "png")
}

svg_ink_box <- \(to_svg, tol = 0.04) {
  raster <- fs::file_temp(ext = "png")
  on.exit(fs::file_delete(raster), add = TRUE)

  to_svg |>
    image_read_svg() |>
    image_write(raster, format = "png")

  pixels <- readPNG(raster)

  if (length(dim(pixels)) == 2L) {
    pixels <- array(pixels, dim = c(dim(pixels), 1L))
  }

  channels <- dim(pixels)[3]
  has_alpha <- channels %in% c(2L, 4L)
  colors <- seq_len(channels - as.integer(has_alpha))

  quantized <- reduce(
    seq_len(channels),
    \(acc, i) acc * 256 + round(pixels[,, i] * 255),
    .init = 0
  )

  tally <- table(quantized)
  background <- which(quantized == as.numeric(names(which.max(tally))))[1]

  visible <- if (has_alpha) {
    pixels[,, channels] > tol
  } else {
    array(TRUE, dim = dim(pixels)[1:2])
  }

  ink <- if (has_alpha && pixels[,, channels][background] <= tol) {
    visible
  } else {
    delta <- reduce(
      colors,
      \(acc, i) pmax(acc, abs(pixels[,, i] - pixels[,, i][background])),
      .init = 0
    )

    visible & delta > tol
  }

  height <- dim(ink)[1]
  width <- dim(ink)[2]

  if (height < 3 || width < 3) {
    return(NULL)
  }

  # rsvg leaves a semi-transparent seam on the outermost rows and columns
  ink[c(1, height), ] <- FALSE
  ink[, c(1, width)] <- FALSE

  rows <- which(apply(ink, 1, any))
  cols <- which(apply(ink, 2, any))

  if (length(rows) == 0 || length(cols) == 0) {
    return(NULL)
  }

  c(
    x0 = (min(cols) - 1) / width,
    x1 = max(cols) / width,
    y0 = (min(rows) - 1) / height,
    y1 = max(rows) / height
  )
}

svg_crop <- \(to_svg, margin = 0.03) {
  lines <- readLines(to_svg)
  header <- grep("<svg[ >]", lines)[1]

  if (is.na(header)) {
    return(invisible(FALSE))
  }

  quoted <- "\\s*=\\s*['\"]([^'\"]*)['\"]"

  view_box <-
    lines[header] |>
    str_extract(paste0("viewBox", quoted), group = 1) |>
    str_squish() |>
    strsplit(" ") |>
    unlist() |>
    as.numeric()

  if (length(view_box) != 4 || anyNA(view_box) || any(view_box[3:4] <= 0)) {
    return(invisible(FALSE))
  }

  box <- svg_ink_box(to_svg)

  if (is.null(box)) {
    return(invisible(FALSE))
  }

  x <- view_box[1] + box[["x0"]] * view_box[3]
  y <- view_box[2] + box[["y0"]] * view_box[4]
  w <- (box[["x1"]] - box[["x0"]]) * view_box[3]
  h <- (box[["y1"]] - box[["y0"]]) * view_box[4]

  pad <- margin * max(w, h)

  x0 <- max(view_box[1], x - pad)
  y0 <- max(view_box[2], y - pad)
  x1 <- min(view_box[1] + view_box[3], x + w + pad)
  y1 <- min(view_box[2] + view_box[4], y + h + pad)

  cropped <- c(x0, y0, x1 - x0, y1 - y0)

  if (isTRUE(all.equal(cropped, view_box, tolerance = 1e-6))) {
    return(invisible(FALSE))
  }

  resize <- \(svg, attribute, index) {
    declared <- str_extract(svg, paste0(attribute, quoted), group = 1)
    value <- as.numeric(str_extract(declared, "^[0-9.]+"))

    if (is.na(value)) {
      return(svg)
    }

    unit <- str_remove(declared, "^[0-9.]+")
    scaled <- cropped[index] * value / view_box[index]

    str_replace(
      svg,
      paste0(attribute, quoted),
      sprintf("%s='%.2f%s'", attribute, scaled, unit)
    )
  }

  lines[header] <-
    lines[header] |>
    str_replace(
      paste0("viewBox", quoted),
      sprintf(
        "viewBox='%.2f %.2f %.2f %.2f'",
        cropped[1],
        cropped[2],
        cropped[3],
        cropped[4]
      )
    ) |>
    resize("width", 3L) |>
    resize("height", 4L)

  # A full-bleed background rect is sized in percent but anchored at the
  # origin, which the translated viewBox leaves behind
  background <- grep("<rect width='100%' height='100%'", lines, fixed = TRUE)[1]

  if (!is.na(background)) {
    lines[background] <- sub(
      "<rect ",
      sprintf("<rect x='%.2f' y='%.2f' ", cropped[1], cropped[2]),
      lines[background],
      fixed = TRUE
    )
  }

  writeLines(lines, to_svg)

  invisible(TRUE)
}
