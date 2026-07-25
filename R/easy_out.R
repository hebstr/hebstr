#' Save a ggplot, gt table, grid graphic, or workbook to disk
#'
#' Export a ggplot, gt, gtsummary, or grid grob object to PNG (and SVG or
#' HTML depending on the object type), or an `openxlsx2` workbook to XLSX.
#' Opens the result in a browser unless `quiet = TRUE`.
#'
#' @param x A ggplot, ggmatrix, gt_tbl, gtsummary, grid grob (for example a
#'   Gmisc flowchart built from `boxGrob()`/`connectGrob()`), or wbWorkbook
#'   object, such as the one [get_xlsx()] returns.
#' @param filename Output filename (without extension). Defaults to the
#'   unevaluated expression passed as `x`.
#' @param dir Output directory. Created if it does not exist. Defaults
#'   to `getOption("easy_out.dir", "output")`.
#' @param suffix Optional suffix appended to `filename`.
#' @param sep Separator between `filename` and `suffix`.
#' @param width Width of the output. For tables: table width in pixels,
#'   overriding the width the object carries from [tbl_format()]. When left
#'   `NULL`, a table declaring its own width keeps it, and one declaring none
#'   gets 700. For plots and grid graphics: SVG width in inches
#'   (default 7). For a grid grob under `crop = TRUE`, `width` and `height`
#'   are a canvas budget rather than the size of the exported file, which
#'   is trimmed back to the drawing it contains.
#' @param height Height in inches for SVG output of plots and grid graphics
#'   only. `NULL` (default) uses the nombre d'or: `width / 1.618`. Ignored
#'   for tables and workbooks.
#' @param px Height in pixels for the PNG rasterization of plots and grid
#'   graphics.
#' @param crop If `TRUE` (the default, read from
#'   `getOption("easy_out.crop")`), trim the SVG canvas of a grid grob to
#'   the bounding box of the drawing, keeping a small margin. Grob positions
#'   are relative to the whole page, so a drawing covering a sub-rectangle
#'   leaves an empty band that no `width`/`height` value removes. Ignored
#'   for tables and plots, whose margins come from the theme, and for
#'   workbooks.
#' @param quiet If `TRUE`, suppress auto-opening the output in a browser. Defaults
#'   to `getOption("easy_out.quiet", FALSE)`.
#' @param export If `FALSE`, return without writing anything. Defaults to
#'   `getOption("easy_out.export")`, itself defaulting to `FALSE` under
#'   `options(hebstr.docx = TRUE)`: a Word run renders tables through
#'   [flextable::flextable()], leaving no object to export to HTML or PNG.
#'   The guard is read from the session option alone, so a workbook exported
#'   during a Word run needs an explicit `export = TRUE`.
#'
#' @details
#' In a remote session, the exported file lives on the server while the
#' browser runs on the local machine, so a file path cannot be opened.
#' `easy_out()` then serves `dir` over HTTP on the loopback interface and
#' opens `http://localhost:<port>/<file>`, which the IDE forwards.
#' The choice is read from `getOption("easy_out.serve")`: `NULL` (the
#' default) detects a remote session through `SSH_CONNECTION`, while `TRUE`
#' or `FALSE` forces the HTTP or the file-path route. Its port comes from
#' `getOption("easy_out.port")`, a free one when unset. The server is started
#' once per session, and restarted whenever `dir` or the requested port
#' changes. Should it fail to start, the file path is opened instead.
#'
#' @return `NULL` (invisibly). Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' easy_out(my_plot)
#' easy_out(my_table, suffix = "v2", quiet = TRUE)
#' easy_out(get_xlsx(list(iris = iris)), filename = "tables")
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
    inherits(x, c("ggmatrix", "gt_tbl", "gtsummary", "wbWorkbook")) ||
    grid::is.grob(x)

  if (!is_supported) {
    cli_abort(c(
      "{.strong {filename}} must be a gt/gtsummary, ggplot, grid grob, or workbook object",
      "i" = "Received object of class: {.cls {class(x)}}",
      if (inherits(x, "flextable")) {
        c(
          "i" = "{.fun tbl_format} returns a {.cls flextable} under {.code options(hebstr.docx = TRUE)}, for Word output. Exporting a {.cls flextable} is out of scope.",
          "i" = "To export, build the table without {.code hebstr.docx} so {.fun tbl_format} returns a {.cls gt_tbl}.",
          "i" = if (getOption("hebstr.docx", default = FALSE)) {
            "To skip the export instead, leave {.arg export} at its {.code hebstr.docx} default."
          } else {
            "To skip the export instead, pass {.code export = FALSE}."
          }
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
      browseURL(browse_url(browse, dir))

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

    gt_width <-
      x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      str_extract("^(\\d+)px$", group = 1) |>
      as.numeric()

    declared <- length(gt_width) > 0 && !is.na(gt_width)

    if (is.null(width)) {
      width <- if (declared) gt_width else 700
    }

    if (!declared || width != gt_width) {
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

    local({
      # closing a device mid-unwind warns "Killing locked device", noise here
      on.exit(suppressWarnings(grDevices::dev.off()), add = TRUE)
      grid::grid.newpage()
      grid::grid.draw(x)
    })

    cli_progress_step("Creating PNG file")

    svg_to_png(to_svg, to_png, px, crop = crop)

    cli_progress_done()

    cli_output(
      files = c(to_svg, to_png),
      browse = to_svg
    )

    ### XLSX -------------------------------------------------------------------------
  } else if (inherits(x, "wbWorkbook")) {
    to_xlsx <- fs::path(path, ext = "xlsx")

    cli_progress_step("Creating XLSX file")

    wb_save(x, file = to_xlsx)

    cli_progress_done()

    cli_output(
      files = to_xlsx,
      browse = to_xlsx
    )
  }

  invisible(NULL)
}

#' Save a list of ggplot or gt objects to disk
#'
#' Iterate over a named list and call [easy_out()] on each element, appending
#' the list name to the filename.
#'
#' @param x A named list of objects [easy_out()] accepts.
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

browse_url <- \(browse, dir) {
  if (!browse_remote()) {
    return(browse)
  }

  port <- browse_server(dir)

  if (is.null(port)) {
    return(browse)
  }

  rel <-
    fs::path_rel(fs::path_abs(browse), start = fs::path_abs(dir)) |>
    strsplit("/", fixed = TRUE) |>
    unlist() |>
    map_chr(URLencode, reserved = TRUE) |>
    paste(collapse = "/")

  sprintf("http://localhost:%s/%s", port, rel)
}

browse_remote <- \() {
  serve <- getOption("easy_out.serve", default = NULL)

  if (!is.null(serve) && !is_bool(serve)) {
    cli_abort(
      "{.code easy_out.serve} must be {.code TRUE}, {.code FALSE}, or {.code NULL}."
    )
  }

  if (is_bool(serve)) {
    return(serve)
  }

  nzchar(Sys.getenv("SSH_CONNECTION"))
}

browse_server <- \(dir) {
  dir <- fs::path_abs(dir)
  port <- getOption("easy_out.port", default = NULL)
  server <- .hebstr$.server
  alive <- !is.null(server) && server$handle$isRunning()

  kept <-
    alive &&
    identical(server$dir, dir) &&
    (is.null(port) || isTRUE(server$handle$getPort() == port))

  if (kept) {
    return(server$handle$getPort())
  }

  if (alive) {
    httpuv::stopServer(server$handle)
  }

  .hebstr$.server <- NULL

  handle <- tryCatch(
    suppressMessages(httpuv::runStaticServer(
      dir = dir,
      host = "127.0.0.1",
      port = port,
      background = TRUE,
      browse = FALSE
    )),
    error = \(cnd) {
      cli_inform(
        message = c(
          "!" = "Could not serve {.path {dir}} over HTTP, opening the file path instead.",
          "i" = conditionMessage(cnd),
          "i" = cli::col_grey(
            "A browser running outside this machine cannot reach a file path."
          )
        ),
        .frequency = "once",
        .frequency_id = "easy_out_serve_fallback"
      )

      NULL
    }
  )

  if (is.null(handle)) {
    return(NULL)
  }

  .hebstr$.server <- list(dir = dir, handle = handle)

  handle$getPort()
}

browse_stop <- \() {
  server <- .hebstr$.server

  if (!is.null(server) && server$handle$isRunning()) {
    httpuv::stopServer(server$handle)
  }

  .hebstr$.server <- NULL

  invisible(NULL)
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
  on.exit(unlink(raster), add = TRUE)

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

  values <- unique(as.vector(quantized))
  modal <- values[which.max(tabulate(match(quantized, values)))]
  background <- which(quantized == modal)[1]

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
