#' Save a ggplot, gt table, grid graphic, widget, or workbook to disk
#'
#' Export a ggplot, gt, gtsummary, or grid grob object to PNG (and SVG or
#' HTML depending on the object type), an htmlwidget to HTML, or an `openxlsx2`
#' workbook to XLSX. A figure also goes to an editable PPTX slide under
#' `pptx = TRUE`, and the variable dictionary [get_vars_dict()] returns also
#' goes to XLSX and JSON. A named list of such objects is written element by
#' element into a folder of its own. Opens the result in a browser unless
#' `quiet = TRUE`.
#'
#' @param x A ggplot, ggmatrix, gt_tbl, gtsummary, grid grob (for example a
#'   Gmisc flowchart built from `boxGrob()`/`connectGrob()`), htmlwidget (a
#'   [reactable::reactable()], say), `hebstr_dict` (what [get_vars_dict()]
#'   returns), or wbWorkbook object, such as the one [get_xlsx()] returns.
#'
#'   A bare named list of such objects is written element by element, each
#'   file taking `sep` and the element name after `filename`, all of them
#'   sharing the folder the list derives. The names are folded into kebab-case
#'   along with the rest of the filename, so they have to stay distinct once
#'   folded. Only a list of class `"list"` is taken this way: an object
#'   carrying a class of its own is one output, list though it may be, which
#'   is what sends a `hebstr_dict` down its own branch rather than into the
#'   walk.
#' @param filename Output filename (without extension). Defaults to the name
#'   of the object passed as `x`, and is required when `x` is anything else
#'   than a name: the deparsed call would name the folder and the file after
#'   the whole expression. Written out in kebab-case: lowercased, with every
#'   run of non-alphanumeric characters folded into a single dash, so
#'   `tbl_demo` writes `tbl-demo.html`.
#' @param dir Output directory. Created if it does not exist. Defaults
#'   to `getOption("easy_out.dir", "output")`.
#' @param subdir Folder created inside `dir` to hold this output. `TRUE` (the
#'   default) derives it from `filename` by that same rule, so `fig_surv_strata`
#'   writes `fig-surv-strata/fig-surv-strata.svg`. The derivation happens
#'   before `suffix` is appended, so the variants of one output share a
#'   folder. For a list it happens once, from the base name rather than from
#'   the name each element carries, so the elements stay grouped:
#'   `fig-surv-strata/fig-surv-strata-os.svg`. `FALSE` writes straight into
#'   `dir`. A string names the folder itself, taken as given.
#' @param suffix Optional suffix appended to `filename`.
#' @param sep Separator between `filename` and `suffix`, and between
#'   `filename` and the element name for a list. Folded into a dash along
#'   with the rest of the name, so it separates without surviving verbatim.
#' @param width Width of the output. For tables: table width in pixels,
#'   overriding the width the object carries from [tbl_format()]. When left
#'   `NULL`, a table declaring its own width keeps it, and one declaring none
#'   gets 700. For plots and grid graphics: SVG width in inches
#'   (default 7). For a grid grob under `crop = TRUE`, `width` and `height`
#'   are a canvas budget rather than the size of the exported file, which
#'   is trimmed back to the drawing it contains. Ignored for widgets, which
#'   carry their own layout.
#' @param height Height in inches for SVG output of plots and grid graphics
#'   only. `NULL` (default) uses the nombre d'or: `width / 1.618`. Ignored
#'   for tables, widgets and workbooks.
#' @param px Height in pixels for the PNG rasterization of plots and grid
#'   graphics.
#' @param crop If `TRUE` (the default, read from
#'   `getOption("easy_out.crop")`), trim the SVG canvas of a grid grob to
#'   the bounding box of the drawing, keeping a small margin. Grob positions
#'   are relative to the whole page, so a drawing covering a sub-rectangle
#'   leaves an empty band that no `width`/`height` value removes. Ignored
#'   for tables and plots, whose margins come from the theme, and for widgets
#'   and workbooks. Ignored for the PPTX slide too, which carries the whole
#'   canvas: an editable object is cropped in the tool that opens it.
#' @param pptx If `TRUE`, also write the plot or grid graphic to a PPTX slide,
#'   as an editable DrawingML shape rather than an image. Defaults to
#'   `getOption("easy_out.pptx", FALSE)`, and requires the \pkg{rvg} package.
#'   The slide is the default 4:3 `Office Theme` one, and the drawing is
#'   scaled to fit it and centred. Fonts are named in the slide rather than
#'   embedded, so a reader without the family installed gets a substitute.
#'   Tables, widgets and workbooks have no slide form and error out.
#' @param quiet If `TRUE`, suppress auto-opening the output in a browser. Defaults
#'   to `getOption("easy_out.quiet", FALSE)`.
#' @param export If `FALSE`, return without writing anything. Defaults to
#'   `getOption("easy_out.export")`, itself defaulting to `FALSE` under
#'   `options(hebstr.docx = TRUE)`: a Word run renders tables through
#'   [flextable::flextable()], leaving no object to export to HTML or PNG.
#'   The guard is read from the session option alone, so a workbook exported
#'   during a Word run needs an explicit `export = TRUE`.
#' @param web_fonts Faces embedded into the SVG, as [svglite::font_face()]
#'   blocks. Defaults to the bundled faces for the font `set_opts()` resolved,
#'   and to `NULL` for a family the package does not ship, which leaves the SVG
#'   rendering in whatever the reader has installed. Supply your own to cover
#'   another family; build them with `woff2` given as a `data:` URI rather than
#'   `embed = TRUE`, which re-emits the face as uncompressed TTF.
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
#' `subdir` never restarts it: the root served stays `dir`, and the folder
#' comes through as a path segment of the URL.
#'
#' @return `NULL` (invisibly). Called for its side effects.
#' @export
#'
#' @examples
#' \dontrun{
#' easy_out(my_plot)
#' easy_out(my_table, suffix = "v2", quiet = TRUE)
#' easy_out(get_xlsx(list(iris = iris)), filename = "tables")
#' easy_out(list(os = p1, pfs = p2), filename = "fig_surv")
#' }
#'
easy_out <- \(
  x,
  filename = NULL,
  dir = getOption("easy_out.dir", default = "output"),
  subdir = TRUE,
  suffix = "",
  sep = "_",
  width = NULL,
  height = NULL,
  px = 1200,
  crop = getOption("easy_out.crop", default = TRUE),
  pptx = getOption("easy_out.pptx", default = FALSE),
  quiet = getOption("easy_out.quiet", default = FALSE),
  export = getOption("easy_out.export", default = !.is_docx()),
  web_fonts = .web_fonts()
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

  if (!is_bool(pptx)) {
    cli_abort("{.arg pptx} must be {.code TRUE} or {.code FALSE}.")
  }

  .check_subdir(subdir)

  check_affix <- \(value, arg) {
    if (length(value) != 1L || is.na(value)) {
      cli_abort("{.arg {arg}} must be a single non-missing value.")
    }
  }

  check_affix(suffix, "suffix")
  check_affix(sep, "sep")

  if (!export) {
    return(invisible(NULL))
  }

  clear_vars()

  # captured before the guards force x, after which enexpr() hands back the value
  x_expr <- enexpr(x)
  label <- as_label(x_expr)

  # bare only: four of the supported classes are named lists themselves, so a
  # class of its own marks one output rather than a list of them
  if (is_bare_list(x)) {
    if (!is_named(x)) {
      cli_abort("{.arg x} must be a named list.")
    }

    if (is.null(filename)) {
      filename <- .out_label(x_expr)
    }

    bases <- map_chr(names(x), ~ .out_name(paste0(filename, sep, .x)))

    if (anyDuplicated(bases)) {
      cli_abort(c(
        "{.arg x} holds element names that fold onto one filename.",
        "i" = "Folded to: {.val {unique(bases[duplicated(bases)])}}.",
        "i" = "Element names are written in kebab-case, so they have to stay distinct once folded."
      ))
    }

    # derived once from the base name, so the elements share a folder
    subdir <- .out_subdir(filename, subdir)

    # elements bound by name, so each announces itself instead of a loop variable
    frame <- env(current_env(), !!!x)

    # for, not walk(): purrr wraps an element error in map()'s indexed condition,
    # reporting a function the caller never called
    for (nm in names(x)) {
      eval_bare(
        expr(easy_out(
          x = !!sym(nm),
          filename = !!paste0(filename, sep, nm),
          dir = !!dir,
          subdir = !!subdir,
          suffix = !!suffix,
          sep = !!sep,
          width = !!width,
          height = !!height,
          px = !!px,
          crop = !!crop,
          pptx = !!pptx,
          quiet = !!quiet,
          export = !!export,
          web_fonts = !!web_fonts
        )),
        env = frame
      )
    }

    return(invisible(NULL))
  }

  cli_h1("easy_out")
  cat_line()

  cli_alert_info("Object: {.strong {label}} {.cls {class(x)}}")
  cat_line()

  is_supported <-
    is_ggplot(x) ||
    inherits(
      x,
      c(
        "ggmatrix",
        "gt_tbl",
        "gtsummary",
        "wbWorkbook",
        "hebstr_dict",
        "htmlwidget"
      )
    ) ||
    grid::is.grob(x)

  if (!is_supported) {
    cli_abort(c(
      "{.strong {label}} must be a gt/gtsummary, ggplot, grid grob, widget, or workbook object, or a named list of them",
      "i" = "Received object of class: {.cls {class(x)}}",
      if (inherits(x, "flextable")) {
        c(
          "i" = "{.fun tbl_format} returns a {.cls flextable} under {.code options(hebstr.docx = TRUE)}, for Word output. Exporting a {.cls flextable} is out of scope.",
          "i" = "To export, build the table without {.code hebstr.docx} so {.fun tbl_format} returns a {.cls gt_tbl}.",
          "i" = if (.is_docx()) {
            "To skip the export instead, leave {.arg export} at its {.code hebstr.docx} default."
          } else {
            "To skip the export instead, pass {.code export = FALSE}."
          }
        )
      }
    ))
  }

  is_figure <- is_ggplot(x) || inherits(x, "ggmatrix") || grid::is.grob(x)

  if (pptx && !is_figure) {
    cli_abort(c(
      "{.arg pptx} only covers a plot or a grid grob.",
      "i" = "Received object of class: {.cls {class(x)}}",
      "i" = "Pass {.code pptx = FALSE} to export {.strong {label}} without a slide."
    ))
  }

  if (is.null(filename)) {
    filename <- .out_label(x_expr)
  }

  .out_name(filename)

  name <- .out_subdir(filename, subdir)
  out_dir <- if (isFALSE(name)) fs::path(dir) else fs::path(dir, name)

  fs::dir_create(out_dir)

  if (nzchar(suffix)) {
    filename <- paste0(filename, sep, suffix)
  }

  path <- fs::path(out_dir, .out_name(filename))
  to_png <- fs::path(path, ext = "png")

  cli_output <- \(files, browse) {
    files_list <- map_chr(
      files,
      ~ format_inline("{cli::col_br_red(fs::path_file(.))}")
    )

    cat_line()
    cli_alert_info("Files saved in {.strong {.path {fs::path_abs(out_dir)}}}")
    cat_line()
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
      height = height,
      system_fonts = .device_fonts(),
      web_fonts = web_fonts
    )

    cli_progress_step("Creating PNG file")

    svg_to_png(to_svg, to_png, px)

    to_pptx <- .out_pptx(pptx, path, \() print(x), width, height)

    cli_progress_done()

    cli_output(
      files = c(to_svg, to_png, to_pptx),
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

    svglite::svglite(
      to_svg,
      width = width,
      height = height,
      system_fonts = .device_fonts(),
      web_fonts = web_fonts
    )

    local({
      # closing a device mid-unwind warns "Killing locked device", noise here
      on.exit(suppressWarnings(grDevices::dev.off()), add = TRUE)
      grid::grid.newpage()
      grid::grid.draw(x)
    })

    cli_progress_step("Creating PNG file")

    svg_to_png(to_svg, to_png, px, crop = crop)

    draw <- \() {
      grid::grid.newpage()
      grid::grid.draw(x)
    }

    to_pptx <- .out_pptx(pptx, path, draw, width, height)

    cli_progress_done()

    cli_output(
      files = c(to_svg, to_png, to_pptx),
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

    ### WIDGET -------------------------------------------------------------------------
  } else if (inherits(x, c("hebstr_dict", "htmlwidget"))) {
    is_dict <- inherits(x, "hebstr_dict")
    widget <- if (is_dict) x$output else x

    to_html <- fs::path(path, ext = "html")

    cli_progress_step("Creating HTML file")

    local({
      # saveWidget resolves libdir against the working directory rather than
      # the file, so its own cleanup misses a libdir written elsewhere
      withr::local_dir(out_dir)
      saveWidget(
        widget,
        file = fs::path_file(to_html),
        selfcontained = .self_contained()
      )
    })

    to_xlsx <- NULL
    to_json <- NULL

    if (is_dict) {
      cli_progress_step("Creating XLSX file")

      to_xlsx <- fs::path(path, ext = "xlsx")
      sheets <- set_names(list(x$data), .out_sheet(fs::path_file(path)))

      wb_save(get_xlsx(sheets, halign = .dict_halign()), file = to_xlsx)

      cli_progress_step("Creating JSON file")

      to_json <- fs::path(path, ext = "json")

      # auto_unbox is what makes the I() marking of .dict_json() load-bearing:
      # without it every scalar comes out as a one-element array instead
      write_json(
        x$json,
        path = to_json,
        pretty = TRUE,
        auto_unbox = TRUE,
        digits = 3,
        na = "null"
      )
    }

    cli_progress_done()

    cli_output(
      files = c(to_html, to_xlsx, to_json),
      browse = to_html
    )
  }

  invisible(NULL)
}

.check_subdir <- \(subdir) {
  valid <-
    is_bool(subdir) ||
    (is_scalar_character(subdir) && !is.na(subdir) && nzchar(subdir))

  if (!valid) {
    cli_abort(
      "{.arg subdir} must be {.code TRUE}, {.code FALSE}, or a folder name."
    )
  }

  invisible(NULL)
}

.out_subdir <- \(filename, subdir) {
  .check_subdir(subdir)

  if (isFALSE(subdir)) {
    return(FALSE)
  }

  if (is_scalar_character(subdir)) {
    return(subdir)
  }

  .out_name(filename)
}

# the deparsed call of a piped or inline argument names the callee as much as
# the object, and a folder is created from it, so it is refused rather than folded
.out_label <- \(expr) {
  label <- as_label(expr)

  if (!is_symbol(expr)) {
    folded <- .kebab(label)

    cli_abort(c(
      "{.arg filename} is required when {.arg x} is not a named object.",
      "i" = "{.arg x} was given as {.code {label}}, which would name a folder and a file {.val {folded}}.",
      "i" = "Assign the object first, or pass {.arg filename} to name the output."
    ))
  }

  label
}

.out_name <- \(filename) {
  if (!is_scalar_character(filename) || is.na(filename)) {
    cli_abort("{.arg filename} must be a single string.")
  }

  name <- .kebab(filename)

  if (!nzchar(name)) {
    cli_abort(c(
      "{.arg filename} holds nothing a name can be built from: {.val {filename}}.",
      "i" = "A folder and a file are both named after it, so it needs at least one alphanumeric character."
    ))
  }

  name
}

# not str_to_kebab(), which splits letter-digit boundaries: a numeric token
# belongs to the name it qualifies ("fig_km_5y" is one output, not five)
.kebab <- \(x) {
  x |>
    str_replace_all("[^[:alnum:]]+", "-") |>
    str_remove_all("^-+|-+$") |>
    str_to_lower()
}

# selfcontained routes through pandoc, absent from a bare R install: the libdir
# fallback keeps the widget readable rather than aborting the export
.self_contained <- \() {
  if (is_installed("rmarkdown") && rmarkdown::pandoc_available()) {
    return(TRUE)
  }

  cli_warn(c(
    "Writing the widget beside its library folder: {.pkg pandoc} was not found.",
    "i" = "A self-contained file needs {.pkg pandoc}, which {.fun htmlwidgets::saveWidget} reaches through {.pkg rmarkdown}.",
    "i" = "The file stays readable in place, but moving it means moving the folder beside it."
  ))

  FALSE
}

# Excel caps a sheet name at 31 characters
.out_sheet <- \(name) str_sub(name, 1, 31)

# rvg names the font family in the slide rather than embedding it, so the
# alias is what keeps the slide and the SVG on the same one
.out_pptx <- \(pptx, path, draw, width, height) {
  if (!pptx) {
    return(NULL)
  }

  check_installed("rvg", reason = "to write a PPTX slide.")

  cli_progress_step("Creating PPTX file")

  to_pptx <- fs::path(path, ext = "pptx")

  doc <- add_slide(read_pptx(), layout = "Blank", master = "Office Theme")
  slide <- slide_size(doc)

  scale <- min(slide$width / width, slide$height / height)
  w <- width * scale
  h <- height * scale

  location <- ph_location(
    left = (slide$width - w) / 2,
    top = (slide$height - h) / 2,
    width = w,
    height = h
  )

  doc |>
    ph_with(
      value = rvg::dml(code = draw(), fonts = .device_fonts()),
      location = location
    ) |>
    print(target = to_pptx)

  to_pptx
}

#' Build a grid graphic against the device that will draw it
#'
#' Opens an off-screen device of the given size, evaluates `code` on it, then
#' closes it and restores the device that was current before. Pass the size
#' the graphic will be exported at, so that a layout computed while the grob
#' is built matches the canvas it ends up on.
#'
#' @param width,height Device size in inches. Pass the values [easy_out()]
#'   will be called with.
#' @param code Expression building the graphic. Evaluated once, on the device
#'   this function opens.
#'
#' @return The value of `code`.
#' @export
#'
#' @details
#' `grid` resolves a unit against the device current at the moment of the
#' conversion, and a package that precomputes layout, `Gmisc` among them,
#' performs that conversion when the grob is created rather than when it is
#' drawn. A `Gmisc::boxGrob()` built in the IDE plot pane therefore carries
#' coordinates measured against the pane, and drawing it on the canvas
#' [easy_out()] opens displaces everything derived from a box edge:
#' `Gmisc::connectGrob(type = "N")` puts its horizontal segment halfway
#' between two boxes, and that half-distance shrinks as the construction
#' device gets shorter.
#'
#' The symptom is one script whose figure differs between the plot pane, a
#' 'Quarto' render and the exported file. Nothing errors, and the boxes
#' themselves look right, their text being measured in absolute units.
#'
#' @section Device family:
#' Text metrics differ from one device family to the next, so the construction
#' device has to come from the same family as the export. Measured on a
#' five-line box at 11 pt: `svglite` 156.43 pt wide, `cairo_pdf` 157.60 pt,
#' `ragg` 156.38 pt, and `pdf(NULL)` 150.08 pt, the last one reaching no
#' system font at all. [withr::with_svg()] wraps the cairo device and so does
#' not qualify here.
#'
#' `svglite::svgstring()` renders to a memory buffer rather than a file. It
#' measures identically to `svglite::svglite()` and writes nothing to disk.
#'
#' @section Cleanup:
#' The device is closed by its number, captured on opening. Closing whichever
#' device happens to be current on exit would close the wrong one as soon as
#' `code` leaves a device behind, which any error raised after a graphics call
#' does.
#'
#' @examples
#' \dontrun{
#' size <- list(width = 11.5, height = 8)
#'
#' flowchart <- with_fig_device(size$width, size$height, {
#'   screened <- Gmisc::boxGrob("Screened (n = 333)", x = 0.4, y = 0.7)
#'   included <- Gmisc::boxGrob("Included (n = 103)", x = 0.4, y = 0.3)
#'
#'   grid::grobTree(
#'     screened,
#'     included,
#'     Gmisc::connectGrob(screened, included, type = "N")
#'   )
#' })
#'
#' easy_out(flowchart, width = size$width, height = size$height)
#' }
#'
#' @seealso [easy_out()], which draws the graphic on a device of that size.
#'
with_fig_device <- \(width, height, code) {
  check_size <- \(value, arg) {
    valid <-
      (is_scalar_double(value) || is_scalar_integer(value)) &&
      !is.na(value) &&
      value > 0

    if (!valid) {
      cli_abort("{.arg {arg}} must be a single positive number, in inches.")
    }
  }

  check_size(width, "width")
  check_size(height, "height")

  previous <- grDevices::dev.cur()

  svglite::svgstring(
    width = width,
    height = height,
    system_fonts = .device_fonts()
  )
  device <- grDevices::dev.cur()

  on.exit(
    {
      grDevices::dev.off(device)

      # dev.set(1) opens a device instead of selecting the null one
      if (previous != 1L) {
        grDevices::dev.set(previous)
      }
    },
    add = TRUE
  )

  code
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
