#' Save a ggplot, table, grid graphic, widget, or workbook to disk
#'
#' Export a ggplot, gt, gtsummary, or grid grob object to PNG (and SVG or
#' HTML depending on the object type), a flextable to DOCX, an htmlwidget to
#' HTML, or an `openxlsx2` workbook to XLSX. A figure also goes to an editable
#' PPTX slide under `pptx = TRUE`, a widget to PNG under `png = TRUE`, and the
#' variable dictionary [get_vars_dict()] returns also goes to XLSX and JSON. A
#' named list of such objects is written element by element into a folder of
#' its own. Opens the result in a browser unless `quiet = TRUE`.
#'
#' @param x A ggplot, ggmatrix, gt_tbl, gtsummary, flextable (what
#'   [tbl_format()] returns under `options(hebstr.docx = TRUE)`), grid grob
#'   (for example a Gmisc flowchart built from `boxGrob()`/`connectGrob()`),
#'   htmlwidget (a [reactable::reactable()], say), `hebstr_dict` (what
#'   [get_vars_dict()] returns), or wbWorkbook object, such as the one
#'   [get_xlsx()] returns.
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
#' @param width Width of the output. For gt tables: table width in pixels,
#'   overriding the width the object carries from [tbl_format()]. When left
#'   `NULL`, a table declaring its own width keeps it, and one declaring none
#'   gets 700. For plots and grid graphics: SVG width in inches
#'   (default 7). For a grid grob under `crop = TRUE`, `width` and `height`
#'   are a canvas budget rather than the size of the exported file, which
#'   is trimmed back to the drawing it contains. For a widget under
#'   `png = TRUE`: the viewport width in pixels the capture composes at
#'   (default 950), the HTML itself carrying its own layout. Ignored for a
#'   flextable, whose width is a fraction of the page that only
#'   [tbl_format()] can compute.
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
#' @param png If `TRUE`, also capture a widget, or the dictionary
#'   [get_vars_dict()] returns, to PNG from the standalone
#'   HTML this branch has just written. Defaults to
#'   `getOption("easy_out.png", FALSE)`, and needs a Chrome installation,
#'   which \pkg{chromote} locates on its own.
#'
#'   Off by default because the package cannot tell whether the image lies: a
#'   frame of an interactive widget freezes the search box and the filter row
#'   and stops at the first page, so a hundred rows look complete. Ask for it
#'   on a widget carrying its whole content, `pagination = FALSE` and no
#'   filters. The other classes either write their PNG already or have no
#'   image form, and ignore it.
#' @param quiet If `TRUE`, suppress auto-opening the output in a browser. Defaults
#'   to `getOption("easy_out.quiet", FALSE)`.
#' @param export If `FALSE`, return without writing anything. The arguments
#'   are checked first, so a call the Word render skips still fails on an
#'   unsupported class, on a slide asked of a table, or on a name nothing can
#'   be built from: one source feeds both renders, and a call has to fail the
#'   same way under each. Defaults to
#'   `getOption("easy_out.export")`, itself defaulting to `FALSE` under
#'   `options(hebstr.docx = TRUE)`: under a Word render the table goes into
#'   the document rather than into a file of its own. The guard is read from
#'   the session option alone, never from the class of `x`, so writing
#'   anything at all during a Word run takes an explicit `export = TRUE`.
#'   That is the way to get a standalone `.docx` out of a script: set both
#'   options for the run, and the flextable [tbl_format()] returns is written
#'   beside the outputs of the other scripts.
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
#'
#' # a Word file of every table of scripts/, beside the HTML and PNG of a
#' # plain run: hebstr.docx picks the flextable branch, easy_out.export says
#' # the table is wanted as a file rather than inside a rendered document
#' withr::with_options(
#'   list(hebstr.docx = TRUE, easy_out.export = TRUE),
#'   auto_exec()
#' )
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
  png = getOption("easy_out.png", default = FALSE),
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

  if (!is_bool(png)) {
    cli_abort("{.arg png} must be {.code TRUE} or {.code FALSE}.")
  }

  .check_subdir(subdir)

  check_affix <- \(value, arg) {
    if (length(value) != 1L || is.na(value)) {
      cli_abort("{.arg {arg}} must be a single non-missing value.")
    }
  }

  check_affix(suffix, "suffix")
  check_affix(sep, "sep")

  # captured before the guards force x, after which enexpr() hands back the value
  x_expr <- enexpr(x)
  label <- as_label(x_expr)

  # bare only: five of the supported classes are named lists themselves, so a
  # class of its own marks one output rather than a list of them
  is_list <- is_bare_list(x)

  # the guards run whatever export says: the same source feeds both renders, so
  # a call has to fail the same way under the one that writes and the one that
  # does not, rather than surfacing only when the file is wanted
  if (is_list) {
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

    # every element cleared before the first one is written, the guards being
    # per-element while the folder they would leave behind is shared
    for (nm in names(x)) {
      .out_check(x[[nm]], pptx, nm)
    }
  } else {
    # before .out_label(), so that rejecting an object built at the call site
    # names its class instead of asking for a filename first
    .out_check(x, pptx, label)

    if (is.null(filename)) {
      filename <- .out_label(x_expr)
    }

    .out_name(filename)
  }

  if (!export) {
    return(invisible(NULL))
  }

  clear_vars()

  # asked once the export is decided, so a Word run needs no slide writer, and
  # before any folder is created, so a declined install leaves nothing behind
  if (pptx) {
    check_installed("rvg", reason = "to write a PPTX slide.")
  }

  if (is_list) {
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
          png = !!png,
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

    given <- !is.null(width)

    gt_width <-
      x[["_options"]] |>
      filter(parameter == "table_width") |>
      pull(value) |>
      unlist()

    # "auto" is what gt carries for a table declaring nothing, so any other
    # value is a declaration; only a pixel one can also size the viewport
    declared <- length(gt_width) == 1L && !identical(gt_width, "auto")
    gt_px <- as.numeric(str_extract(gt_width, "^([0-9.]+)px$", group = 1))

    if (!given) {
      width <- if (length(gt_px) == 1L && !is.na(gt_px)) gt_px else 700
    }

    if (given || !declared) {
      x <- x |> tab_options(table.width = px(width))
    }

    # the stylesheet is all that travels with the file, so the faces ride
    # inside it; Chrome reads them too, which carries the PNG with it
    faces <- .css_faces(.gt_family(x))

    if (!is.null(faces)) {
      x <- opt_css(x, css = faces)
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

    ### DOCX -------------------------------------------------------------------------
  } else if (inherits(x, "flextable")) {
    to_docx <- fs::path(path, ext = "docx")

    cli_progress_step("Creating DOCX file")

    .out_docx(x, to_docx)

    cli_progress_done()

    cli_output(
      files = to_docx,
      browse = to_docx
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

    # drawn again rather than rasterised from the SVG: rsvg resolves fonts
    # through fontconfig alone, so a family the package registered or a reader
    # supplied lands on a fallback there, silently. ragg reads the same
    # systemfonts resolution svglite used, which is what keeps the two files
    # showing the same type.
    ggsave(
      filename = to_png,
      plot = x,
      device = ragg::agg_png,
      width = width,
      height = height,
      dpi = px / height
    )

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

    to_shot <- .out_shot(png, to_html, to_png, width)

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
      files = c(to_html, to_shot, to_xlsx, to_json),
      browse = to_html
    )
  }

  invisible(NULL)
}

# the coherence of the call, gathered so that the list branch can clear every
# element before writing the first. Availability of a suggested package is not
# checked here: it belongs to the run that writes, not to the call. A bare list
# is left to the recursion, which checks the elements it fans out.
.out_check <- \(x, pptx, label) {
  if (is_bare_list(x)) {
    return(invisible(NULL))
  }

  is_supported <-
    is_ggplot(x) ||
    inherits(
      x,
      c(
        "ggmatrix",
        "gt_tbl",
        "gtsummary",
        "flextable",
        "wbWorkbook",
        "hebstr_dict",
        "htmlwidget"
      )
    ) ||
    grid::is.grob(x)

  if (!is_supported) {
    cli_abort(c(
      "{.strong {label}} must be a gt/gtsummary/flextable, ggplot, grid grob, widget, or workbook object, or a named list of them",
      "i" = "Received object of class: {.cls {class(x)}}"
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

  cli_warn(
    c(
      "Writing the widget beside its library folder: {.pkg pandoc} was not found.",
      "i" = "A self-contained file needs {.pkg pandoc}, which {.fun htmlwidgets::saveWidget} reaches through {.pkg rmarkdown}.",
      "i" = "The file stays readable in place, but moving it means moving the folder beside it."
    ),
    .frequency = "once",
    .frequency_id = "easy_out_self_contained"
  )

  FALSE
}

# Excel caps a sheet name at 31 characters
.out_sheet <- \(name) str_sub(name, 1, 31)

# The clip rectangle is measured on a page that still reserves the scrollbar,
# while captureBeyondViewport widens the viewport to paint: Chrome then rewraps
# on those fifteen pixels and every full line loses its end, silently. Hiding
# the bar beforehand makes the two widths agree. chromote's own
# setScrollbarsHidden does not reflow, so it does not settle this.
.out_shot <- \(png, to_html, to_png, width) {
  if (!png) {
    return(NULL)
  }

  cli_progress_step("Creating PNG file")

  if (is.null(width)) {
    width <- 950
  }

  # the capture decorates an export already on disk, and the files of the
  # dictionary are written after it: a missing browser or an unanswered
  # navigation throws, and would take them down with it
  failed <- tryCatch(
    {
      session <- ChromoteSession$new(width = width, height = 800)
      on.exit(session$close(), add = TRUE)

      # go_to(), not navigate() then loadEventFired(): the event is registered
      # before the navigation, where the pair races it and times out on a fast load
      session$go_to(paste0(
        "file://",
        URLencode(as.character(fs::path_abs(to_html)))
      ))

      session$Runtime$evaluate(
        "document.documentElement.style.overflowY = 'hidden'"
      )

      # the widget is drawn by the script the page carries, after the load event
      session$screenshot(
        to_png,
        selector = ".html-widget",
        scale = 2,
        expand = 5,
        delay = 2
      )

      NULL
    },
    error = \(cnd) cnd
  )

  # chromote turns a failed capture into a warning, which would leave the
  # banner announcing a file that was never written
  if (!is.null(failed) || !fs::file_exists(to_png)) {
    cli_warn(
      c(
        "No PNG was captured from {.path {fs::path_file(to_html)}}.",
        "i" = "The page has to reach Chrome: a sandboxed install (snap, flatpak) has a private {.path /tmp} and sees nothing written there.",
        "i" = "The HTML file itself was written and is unaffected."
      ),
      parent = failed
    )

    return(NULL)
  }

  to_png
}

# rvg names the font family in the slide rather than embedding it, so the
# alias is what keeps the slide and the SVG on the same one
.out_pptx <- \(pptx, path, draw, width, height) {
  if (!pptx) {
    return(NULL)
  }

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


# A run names one font and OOXML has no stack, so a family the reader has not
# installed is replaced by whatever Word's metric matching returns. What the
# document can say about it lives in two parts a themed flextable never
# reaches, both written here: the faces themselves, embedded as obfuscated
# parts, and the ordered alternatives of w:altName.
#
# body_add_flextable() defaults align to NULL, where save_as_docx() centres the
# table and overrides the alignment the object carries.
.out_docx <- \(x, path) {
  families <- .ft_families(x)

  doc <- body_add_flextable(read_docx(), x)
  doc <- reduce(families, .docx_embed, .init = doc)

  print(doc, target = path)

  .docx_alt_name(path, families)

  path
}


# the family the object carries rather than the session default: easy_out()
# receives a table someone else themed, possibly under another font
.gt_family <- \(x) {
  family <-
    x[["_options"]] |>
    filter(parameter == "table_font_names") |>
    pull(value) |>
    unlist()

  if (length(family)) family[[1]] else NULL
}


.ft_families <- \(x) {
  parts <- c("header", "body", "footer")
  families <- map(parts, ~ as.vector(x[[.x]]$styles$text$font.family$data))

  sort(unique(unlist(families)), na.last = NA)
}


.docx_embed <- \(doc, family) {
  faces <- .font_faces(family)

  if (is.null(faces)) {
    return(doc)
  }

  exec(docx_embed_font, doc, font_family = family, !!!faces)
}


# w:altName is the only fallback OOXML defines, an ordered list the reader tries
# before falling back on metric matching. Word honours it and LibreOffice does
# not (measured), which is what the metric hints of a created entry are for: a
# reader deaf to the list still lands on a variable-pitch sans rather than on a
# serif. Nothing here is exposed by officer, hence the pass over the archive.
.docx_alt_name <- \(path, families) {
  dir <- withr::local_tempdir()
  zip::unzip(path, exdir = dir)

  table <- fs::path(dir, "word", "fontTable.xml")
  doc <- xml2::read_xml(table)

  # the prefix map is built rather than read from the document: officer
  # redeclares the wordprocessing namespace on the nodes it adds, and
  # xml_ns() then binds that one URI to w, w1 and w2, which makes every
  # attribute read come back NA
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  root <- xml2::xml_find_first(doc, "/w:fonts", ns)

  # matched in R rather than through an XPath predicate, which a family name
  # carrying a quote would break
  declared <- xml2::xml_find_all(doc, "//w:font", ns)
  named <- xml2::xml_attr(declared, "w:name", ns)

  # the same ordered fallback the CSS stacks carry, in the comma-delimited
  # form w:altName takes
  alt <- paste(.font_fallback, collapse = ", ")

  walk(families, \(family) {
    hit <- match(family, named)
    node <- if (is.na(hit)) NULL else declared[[hit]]

    if (is.null(node)) {
      node <- xml2::xml_add_child(root, "w:font", "w:name" = family)
      # the CT_Font sequence orders altName first, then the metrics
      xml2::xml_add_child(node, "w:charset", "w:val" = "00")
      xml2::xml_add_child(node, "w:family", "w:val" = "swiss")
      xml2::xml_add_child(node, "w:pitch", "w:val" = "variable")
    }

    if (inherits(xml2::xml_find_first(node, "w:altName", ns), "xml_missing")) {
      xml2::xml_add_child(node, "w:altName", "w:val" = alt, .where = 0)
    }
  })

  xml2::write_xml(doc, table)

  # the package root relationships live in a dotfile, which list.files() skips
  # by default: dropping it leaves an archive no reader opens
  zip::zip(
    fs::path_abs(path),
    list.files(dir, recursive = TRUE, all.files = TRUE),
    root = dir
  )

  path
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
#'   this function opens, so it has to be the construction itself rather than
#'   a graphic built beforehand. A bare symbol is rejected: its value was
#'   computed before the call, which leaves nothing for the device to measure.
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
#' Hoisting the construction into a variable reinstates that symptom, since
#' `code` is then a symbol whose value the enclosing assignment has already
#' forced. Splitting the construction out stays possible through a function,
#' called from `code`:
#'
#' ```
#' .flow_grob <- \() { ... }
#'
#' with_fig_device(width, height, code = .flow_grob())
#' ```
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

  code_expr <- if (missing(code)) NULL else enexpr(code)

  if (is_symbol(code_expr)) {
    name <- as.character(code_expr)

    cli_abort(c(
      "{.arg code} must be the expression building the graphic.",
      "x" = "{.var {name}} is already built when {.fn with_fig_device} is
             called, so the device it opens measures nothing.",
      "i" = "Inline the construction, or make {.var {name}} a function and
             call it: {.code code = {name}()}."
    ))
  }

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
