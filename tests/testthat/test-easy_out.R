test_that("easy_out() rejects unsupported objects", {
  expect_error(
    easy_out(mtcars, quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("easy_out() points at hebstr.docx when handed a flextable", {
  expect_error(
    easy_out(flextable::flextable(head(mtcars)), quiet = TRUE),
    "hebstr.docx"
  )
})

test_that("easy_out() names export = FALSE when hebstr.docx is unset", {
  expect_error(
    easy_out(flextable::flextable(head(mtcars)), quiet = TRUE),
    "export = FALSE",
    fixed = TRUE
  )
})

test_that("easy_out() rejects a non-boolean export", {
  expect_error(
    easy_out(mtcars, export = "yes", quiet = TRUE),
    class = "rlang_error",
    regexp = "`export` must be",
    fixed = TRUE
  )
})

test_that("easy_out() writes nothing when export is FALSE", {
  tmp <- withr::local_tempdir()
  dir <- fs::path(tmp, "skipped")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  expect_null(easy_out(p, filename = "test", dir = dir, export = FALSE))
  expect_false(fs::dir_exists(dir))
})

test_that("easy_out() leaves the variable context intact when export is FALSE", {
  local_opts()
  use_vars(mtcars)

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  easy_out(
    p,
    filename = "test",
    dir = withr::local_tempdir(),
    export = FALSE
  )

  expect_true(exists(".vars_context", envir = .hebstr, inherits = FALSE))
})

test_that("easy_out() skips the export under hebstr.docx rather than rejecting a flextable", {
  tmp <- withr::local_tempdir()
  dir <- fs::path(tmp, "docx")

  withr::local_options(hebstr.docx = TRUE)

  expect_no_error(
    easy_out(flextable::flextable(head(mtcars)), filename = "test", dir = dir)
  )
  expect_false(fs::dir_exists(dir))
})

test_that("easy_out() names hebstr.docx and export when a flextable export is forced", {
  withr::local_options(hebstr.docx = TRUE)

  expect_error(
    easy_out(
      flextable::flextable(head(mtcars)),
      quiet = TRUE,
      export = TRUE
    ),
    "hebstr.docx"
  )
})

test_that("easy_out() names the export default under a Word run", {
  withr::local_options(hebstr.docx = TRUE)

  expect_error(
    easy_out(
      flextable::flextable(head(mtcars)),
      quiet = TRUE,
      export = TRUE
    ),
    "leave `export` at its `hebstr.docx` default",
    fixed = TRUE
  )
})

test_that("easy_out.export option overrides the hebstr.docx default", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  write_called <- FALSE
  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) {
      write_called <<- TRUE
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  withr::local_options(hebstr.docx = TRUE, easy_out.export = TRUE)

  easy_out(p, filename = "test", dir = tmp, quiet = TRUE)

  expect_true(write_called)
})

test_that("easy_out() rejects a vector", {
  expect_error(
    easy_out(1:10, quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("easy_out() accepts a ggplot and creates files", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  write_called <- FALSE
  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) {
      write_called <<- TRUE
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  expect_no_error(
    easy_out(p, filename = "test_plot", dir = tmp, quiet = TRUE)
  )

  expect_true(write_called)
})

test_that("easy_out() routes a ggmatrix through the plot branch", {
  skip_if_not_installed("GGally")

  tmp <- withr::local_tempdir()
  m <- GGally::ggpairs(mtcars[, c("mpg", "hp")])

  captured_plot <- NULL
  local_mocked_bindings(
    ggsave = \(filename, plot, ...) {
      captured_plot <<- plot
      writeLines("<svg ></svg>", filename)
    },
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  expect_no_error(
    easy_out(m, filename = "pairs", dir = tmp, quiet = TRUE)
  )

  expect_s3_class(captured_plot, "ggmatrix")
})

test_that("easy_out() builds filename with suffix", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  captured_svg <- NULL
  local_mocked_bindings(
    ggsave = \(filename, ...) {
      captured_svg <<- filename
      writeLines("<svg ></svg>", filename)
    },
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(p, filename = "myplot", suffix = "v2", dir = tmp, quiet = TRUE)

  expect_match(captured_svg, "myplot_v2")
})

test_that("easy_out() creates output directory if missing", {
  tmp <- withr::local_tempdir()
  output_dir <- fs::path(tmp, "new_folder")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(p, filename = "test", dir = output_dir, quiet = TRUE)

  expect_true(fs::dir_exists(output_dir))
})

test_that("easy_out() accepts a gt_tbl and calls gtsave/webshot", {
  tmp <- withr::local_tempdir()

  gt_obj <- gt::gt(head(mtcars, 3))

  saved_html <- NULL
  webshot_called <- FALSE

  local_mocked_bindings(
    gtsave = \(data, filename, ...) {
      saved_html <<- filename
      writeLines("<html></html>", filename)
    },
    webshot = \(...) {
      webshot_called <<- TRUE
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_table", dir = tmp, quiet = TRUE)

  expect_match(saved_html, "test_table\\.html$")
  expect_true(webshot_called)
})

test_that("easy_out() applies default width to gt when no table_width is set", {
  tmp <- withr::local_tempdir()
  gt_obj <- gt::gt(head(mtcars, 3))

  captured_vwidth <- NULL
  captured_gt <- NULL
  local_mocked_bindings(
    gtsave = \(data, filename, ...) {
      captured_gt <<- data
      writeLines("<html></html>", filename)
    },
    webshot = \(url, file, vwidth, ...) {
      captured_vwidth <<- vwidth
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_w", dir = tmp, quiet = TRUE)

  expect_equal(captured_vwidth, 700 * 1.1)

  applied_width <- captured_gt[["_options"]] |>
    dplyr::filter(parameter == "table_width") |>
    dplyr::pull(value) |>
    unlist()
  expect_identical(applied_width, "700px")
})

test_that("easy_out() uses user-supplied width when gt has no table_width", {
  tmp <- withr::local_tempdir()
  gt_obj <- gt::gt(head(mtcars, 3))

  captured_vwidth <- NULL
  local_mocked_bindings(
    gtsave = \(data, filename, ...) writeLines("<html></html>", filename),
    webshot = \(url, file, vwidth, ...) {
      captured_vwidth <<- vwidth
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_w", dir = tmp, width = 800, quiet = TRUE)

  expect_equal(captured_vwidth, 800 * 1.1)
})

test_that("easy_out() uses gt table_width when set", {
  tmp <- withr::local_tempdir()
  gt_obj <- gt::gt(head(mtcars, 3)) |>
    gt::tab_options(table.width = gt::px(400))

  captured_vwidth <- NULL
  local_mocked_bindings(
    gtsave = \(data, filename, ...) writeLines("<html></html>", filename),
    webshot = \(url, file, vwidth, ...) {
      captured_vwidth <<- vwidth
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_w", dir = tmp, quiet = TRUE)

  expect_equal(captured_vwidth, 400 * 1.1)
})

test_that("easy_out() lets an explicit width override the gt table_width", {
  tmp <- withr::local_tempdir()
  gt_obj <- gt::gt(head(mtcars, 3)) |>
    gt::tab_options(table.width = gt::px(400))

  captured_vwidth <- NULL
  captured_gt <- NULL
  local_mocked_bindings(
    gtsave = \(data, filename, ...) {
      captured_gt <<- data
      writeLines("<html></html>", filename)
    },
    webshot = \(url, file, vwidth, ...) {
      captured_vwidth <<- vwidth
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_w", dir = tmp, width = 900, quiet = TRUE)

  expect_equal(captured_vwidth, 900 * 1.1)

  applied_width <- captured_gt[["_options"]] |>
    dplyr::filter(parameter == "table_width") |>
    dplyr::pull(value) |>
    unlist()
  expect_identical(applied_width, "900px")
})

test_that("easy_out() ignores a percentage table_width and falls back to default px", {
  tmp <- withr::local_tempdir()
  gt_obj <- gt::gt(head(mtcars, 3)) |>
    gt::tab_options(table.width = gt::pct(80))

  captured_vwidth <- NULL
  local_mocked_bindings(
    gtsave = \(data, filename, ...) writeLines("<html></html>", filename),
    webshot = \(url, file, vwidth, ...) {
      captured_vwidth <<- vwidth
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gt_obj, filename = "test_pct", dir = tmp, quiet = TRUE)

  expect_equal(captured_vwidth, 700 * 1.1)
})

test_that("easy_out() rejects a character string", {
  expect_error(
    easy_out("not_a_plot", quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("easy_out() suffix with custom separator", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  captured_svg <- NULL
  local_mocked_bindings(
    ggsave = \(filename, ...) {
      captured_svg <<- filename
      writeLines("<svg ></svg>", filename)
    },
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(
    p,
    filename = "fig",
    suffix = "final",
    sep = "-",
    dir = tmp,
    quiet = TRUE
  )

  expect_match(captured_svg, "fig-final")
})

test_that("easy_out() calls browseURL when quiet is FALSE", {
  withr::local_options(easy_out.serve = FALSE)

  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  browse_called <- FALSE
  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) {
      browse_called <<- TRUE
      invisible(NULL)
    }
  )

  easy_out(p, filename = "test_browse", dir = tmp, quiet = FALSE)

  expect_true(browse_called)
})

test_that("easy_out() holds browseURL back when quiet is TRUE", {
  withr::local_options(easy_out.serve = FALSE)

  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  browse_called <- FALSE
  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) {
      browse_called <<- TRUE
      invisible(NULL)
    }
  )

  easy_out(p, filename = "test_quiet", dir = tmp, quiet = TRUE)

  expect_false(browse_called)
})

test_that("easy_out() accepts a gtsummary object and calls as_gt", {
  tmp <- withr::local_tempdir()

  gtsum_obj <- gtsummary::tbl_summary(head(mtcars, 5), include = mpg)

  as_gt_called <- FALSE
  local_mocked_bindings(
    as_gt = \(x, ...) {
      as_gt_called <<- TRUE
      gt::gt(head(mtcars, 3))
    },
    gtsave = \(data, filename, ...) writeLines("<html></html>", filename),
    webshot = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(gtsum_obj, filename = "test_gtsum", dir = tmp, quiet = TRUE)

  expect_true(as_gt_called)
})

test_that("easy_out() uses option easy_out.dir as default directory", {
  tmp <- withr::local_tempdir()
  custom_dir <- fs::path(tmp, "custom_output")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  withr::local_options(easy_out.dir = custom_dir)

  easy_out(p, filename = "test_dir", quiet = TRUE)

  expect_true(fs::dir_exists(custom_dir))
})

test_that("easy_out() dir argument overrides easy_out.dir option", {
  tmp <- withr::local_tempdir()
  option_dir <- fs::path(tmp, "from_option")
  arg_dir <- fs::path(tmp, "from_arg")

  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  withr::local_options(easy_out.dir = option_dir)

  easy_out(p, filename = "test_dir", dir = arg_dir, quiet = TRUE)

  expect_true(fs::dir_exists(arg_dir))
  expect_false(fs::dir_exists(option_dir))
})

test_that("easy_out() accepts a grid grob and creates an SVG", {
  tmp <- withr::local_tempdir()
  g <- grid::grobTree(grid::rectGrob(), grid::textGrob("x"))

  write_called <- FALSE
  local_mocked_bindings(
    image_read_svg = \(...) "mock_img",
    image_write = \(...) {
      write_called <<- TRUE
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  expect_no_error(
    easy_out(g, filename = "test_grob", dir = tmp, crop = FALSE, quiet = TRUE)
  )

  expect_true(fs::file_exists(fs::path(tmp, "test_grob", ext = "svg")))
  expect_true(write_called)
})

test_that("easy_out() injects xml:space=preserve on the grob SVG", {
  tmp <- withr::local_tempdir()
  g <- grid::grobTree(grid::rectGrob(), grid::textGrob("a    b"))

  local_mocked_bindings(
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(g, filename = "test_space", dir = tmp, crop = FALSE, quiet = TRUE)

  svg <- readLines(fs::path(tmp, "test_space", ext = "svg"))
  expect_match(
    paste(svg, collapse = "\n"),
    'xml:space="preserve"',
    fixed = TRUE
  )
})

test_that("easy_out() builds grob filename with suffix", {
  tmp <- withr::local_tempdir()
  g <- grid::rectGrob()

  local_mocked_bindings(
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  easy_out(
    g,
    filename = "flow",
    suffix = "v2",
    dir = tmp,
    crop = FALSE,
    quiet = TRUE
  )

  expect_true(fs::file_exists(fs::path(tmp, "flow_v2", ext = "svg")))
})

test_that("easy_out() closes the SVG device when the grob fails to draw", {
  tmp <- withr::local_tempdir()
  g <- grid::rectGrob(gp = grid::gpar(fill = "not_a_colour"))

  before <- grDevices::dev.cur()

  expect_error(
    easy_out(g, filename = "boom", dir = tmp, crop = FALSE, quiet = TRUE)
  )

  expect_identical(grDevices::dev.cur(), before)
})

test_that("easy_out_map() rejects an unnamed list", {
  plots <- list(
    ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
      ggplot2::geom_point()
  )

  expect_error(
    easy_out_map(plots),
    class = "rlang_error"
  )
})

test_that("easy_out_map() rejects a data.frame", {
  expect_error(
    easy_out_map(mtcars),
    class = "rlang_error"
  )
})

test_that("easy_out_map() rejects a vector", {
  expect_error(
    easy_out_map(1:5),
    class = "rlang_error"
  )
})

test_that("easy_out_map() iterates over a list of ggplots", {
  tmp <- withr::local_tempdir()

  plots <- list(
    a = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
      ggplot2::geom_point(),
    b = ggplot2::ggplot(mtcars, ggplot2::aes(wt, qsec)) +
      ggplot2::geom_point()
  )

  call_count <- 0L
  local_mocked_bindings(
    easy_out = \(...) {
      call_count <<- call_count + 1L
      invisible(NULL)
    }
  )

  expect_no_error(easy_out_map(plots, filename = "fig", dir = tmp))
  expect_equal(call_count, 2L)
})

test_that("easy_out_map() forwards ... to easy_out()", {
  tmp <- withr::local_tempdir()

  plots <- list(
    a = ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
      ggplot2::geom_point()
  )

  captured_args <- list()
  local_mocked_bindings(
    easy_out = \(...) {
      captured_args <<- list(...)
      invisible(NULL)
    }
  )

  easy_out_map(plots, filename = "fig", dir = tmp, quiet = TRUE, suffix = "v1")

  expect_match(as.character(captured_args$filename), "fig_a")
  expect_equal(captured_args$dir, tmp)
  expect_true(captured_args$quiet)
  expect_equal(captured_args$suffix, "v1")
})

test_that("easy_out() rejects a non-boolean crop", {
  expect_error(
    easy_out(grid::rectGrob(), crop = "yes", quiet = TRUE),
    class = "rlang_error",
    regexp = "`crop` must be",
    fixed = TRUE
  )
})

test_that("svg_ink_box() lets the rasterization error through", {
  path <- .make_svg(grid::rectGrob())

  local_mocked_bindings(image_read_svg = \(...) cli_abort("rsvg failed"))

  expect_error(svg_ink_box(path), "rsvg failed")
})

test_that("svg_crop() trims the canvas to the drawing", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  )

  before <- .view_box(path)

  expect_true(svg_crop(path))

  after <- .view_box(path)

  pad <- 2 * 0.03 * 0.30 * before[3]

  expect_equal(after[3], 0.30 * before[3] + pad, tolerance = 0.02)
  expect_equal(after[4], 0.20 * before[4] + pad, tolerance = 0.03)
  expect_equal(after[1], 0.25 * before[3] - pad / 2, tolerance = 0.02)
})

test_that("svg_crop() pads the cropped canvas by margin on all four sides", {
  grob <- grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  tight <- .make_svg(grob)
  padded <- .make_svg(grob)

  svg_crop(tight, margin = 0)
  svg_crop(padded, margin = 0.05)

  a <- .view_box(tight)
  b <- .view_box(padded)
  pad <- 0.05 * max(a[3], a[4])

  expect_equal(b[1], a[1] - pad)
  expect_equal(b[2], a[2] - pad)
  expect_equal(b[3], a[3] + 2 * pad)
  expect_equal(b[4], a[4] + 2 * pad)
})

test_that("svg_crop() keeps the drawing inside the cropped canvas", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  )

  before <- .view_box(path)
  svg_crop(path)
  after <- .view_box(path)

  ink <- c(0.25, 0.55, 0.30, 0.50) * rep(before[3:4], each = 2)

  expect_lte(after[1], ink[1])
  expect_gte(after[1] + after[3], ink[2])
  expect_lte(after[2], ink[3])
  expect_gte(after[2] + after[4], ink[4])
})

test_that("svg_crop() rewrites the width and height attributes", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  )

  svg_crop(path)

  header <- grepv("<svg[ >]", readLines(path))
  after <- .view_box(path)

  expect_match(header, sprintf("width='%.2fpt'", after[3]), fixed = TRUE)
  expect_match(header, sprintf("height='%.2fpt'", after[4]), fixed = TRUE)
})

test_that("svg_crop() crops a transparent canvas on alpha alone", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2),
    bg = "transparent"
  )

  before <- .view_box(path)

  expect_true(svg_crop(path))
  expect_lt(.view_box(path)[3], before[3])
})

test_that("svg_crop() leaves a full-bleed canvas untouched", {
  path <- .make_svg(
    grid::rectGrob(gp = grid::gpar(fill = "red", col = NA))
  )

  before <- readLines(path)

  expect_false(svg_crop(path))
  expect_identical(readLines(path), before)
})

test_that("svg_crop() leaves an empty canvas untouched", {
  path <- .make_svg(NULL)

  before <- readLines(path)

  expect_false(svg_crop(path))
  expect_identical(readLines(path), before)
})

test_that("svg_crop() leaves an SVG without a viewBox untouched", {
  path <- withr::local_tempfile(fileext = ".svg")
  writeLines("<svg width='10pt' height='10pt'></svg>", path)

  expect_false(svg_crop(path))
  expect_identical(readLines(path), "<svg width='10pt' height='10pt'></svg>")
})

test_that("easy_out() crops the grob canvas by default", {
  tmp <- withr::local_tempdir()
  g <- grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)

  local_mocked_bindings(browseURL = \(...) invisible(NULL))

  easy_out(g, filename = "cropped", dir = tmp, width = 9, quiet = TRUE)

  expect_equal(
    .view_box(fs::path(tmp, "cropped", ext = "svg"))[3],
    0.30 * 9 * 72 * (1 + 2 * 0.03),
    tolerance = 0.02
  )
})

test_that("easy_out() leaves the grob canvas whole when crop is FALSE", {
  tmp <- withr::local_tempdir()
  g <- grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)

  local_mocked_bindings(browseURL = \(...) invisible(NULL))

  easy_out(
    g,
    filename = "whole",
    dir = tmp,
    width = 9,
    crop = FALSE,
    quiet = TRUE
  )

  expect_equal(.view_box(fs::path(tmp, "whole", ext = "svg"))[3], 9 * 72)
})

test_that("easy_out() leaves the plot canvas whole", {
  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(browseURL = \(...) invisible(NULL))

  easy_out(p, filename = "plot", dir = tmp, width = 9, quiet = TRUE)

  expect_equal(.view_box(fs::path(tmp, "plot", ext = "svg"))[3], 9 * 72)
})

test_that("svg_crop() carries the background rect along with the viewBox", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  )

  svg_crop(path)

  raster <- withr::local_tempfile(fileext = ".png")
  magick::image_write(magick::image_read_svg(path), raster, format = "png")

  pixels <- png::readPNG(raster)

  skip_if_not(dim(pixels)[3] %in% c(2L, 4L), "raster carries no alpha")

  clear <- pixels[,, dim(pixels)[3]] < 0.5

  expect_false(any(apply(clear, 2, all)))
  expect_false(any(apply(clear, 1, all)))
})

test_that("easy_out() writes a workbook to a readable xlsx file", {
  tmp <- withr::local_tempdir()
  wb <- get_xlsx(list(iris = head(iris, 3)))

  easy_out(wb, filename = "book", dir = tmp, quiet = TRUE)

  path <- fs::path(tmp, "book", ext = "xlsx")

  expect_true(fs::file_exists(path))
  expect_equal(nrow(openxlsx2::read_xlsx(path)), 3L)
})

test_that("easy_out() creates the output directory for a workbook", {
  tmp <- withr::local_tempdir()
  dir <- fs::path(tmp, "new_folder")

  easy_out(
    get_xlsx(list(a = head(iris, 3))),
    filename = "book",
    dir = dir,
    quiet = TRUE
  )

  expect_true(fs::dir_exists(dir))
})

test_that("easy_out() appends the suffix to a workbook filename", {
  tmp <- withr::local_tempdir()

  easy_out(
    get_xlsx(list(a = head(iris, 3))),
    filename = "book",
    suffix = "v2",
    dir = tmp,
    quiet = TRUE
  )

  expect_true(fs::file_exists(fs::path(tmp, "book_v2", ext = "xlsx")))
})

test_that("easy_out() holds the session guard for a workbook", {
  tmp <- withr::local_tempdir()
  wb <- get_xlsx(list(a = head(iris, 3)))

  withr::local_options(hebstr.docx = TRUE)

  easy_out(wb, filename = "skipped", dir = tmp, quiet = TRUE)
  expect_false(fs::file_exists(fs::path(tmp, "skipped", ext = "xlsx")))

  easy_out(wb, filename = "forced", dir = tmp, quiet = TRUE, export = TRUE)
  expect_true(fs::file_exists(fs::path(tmp, "forced", ext = "xlsx")))
})

test_that("browse_url() hands back the file path outside a remote session", {
  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "report", ext = "html")

  withr::local_options(easy_out.serve = FALSE)

  expect_identical(browse_url(target, tmp), target)
  expect_null(.hebstr$.server)
})

test_that("browse_url() serves the output directory in a remote session", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "report", ext = "html")

  withr::local_options(easy_out.serve = TRUE)

  expect_match(
    browse_url(target, tmp),
    "^http://localhost:[0-9]+/report\\.html$"
  )
  expect_identical(.hebstr$.server$dir, fs::path_abs(tmp))
})

test_that("browse_url() percent-encodes the file name", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "my report", ext = "html")

  withr::local_options(easy_out.serve = TRUE)

  expect_match(browse_url(target, tmp), "/my%20report\\.html$")
})

test_that("browse_url() percent-encodes the reserved characters", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "a#b&c", ext = "html")

  withr::local_options(easy_out.serve = TRUE)

  expect_match(browse_url(target, tmp), "/a%23b%26c\\.html$")
})

test_that("browse_url() points at a URL the browser can fetch", {
  skip_on_cran()
  skip_if_not(capabilities("libcurl"))

  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "report", ext = "html")
  writeLines("<p>served</p>", target)

  withr::local_options(easy_out.serve = TRUE)

  url <- browse_url(target, tmp)

  expect_match(curlGetHeaders(url)[1], "200 OK")
  expect_identical(readLines(url, warn = FALSE), "<p>served</p>")
})

test_that("browse_url() reuses a single server across calls", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()

  withr::local_options(easy_out.serve = TRUE)

  first <- browse_url(fs::path(tmp, "a", ext = "html"), tmp)
  handle <- .hebstr$.server$handle
  second <- browse_url(fs::path(tmp, "b", ext = "html"), tmp)

  expect_identical(.hebstr$.server$handle, handle)
  expect_identical(
    sub("/[^/]+$", "", first),
    sub("/[^/]+$", "", second)
  )
})

test_that("browse_url() restarts the server when the directory changes", {
  skip_on_cran()

  first_dir <- withr::local_tempdir()
  second_dir <- withr::local_tempdir()
  local_server()

  withr::local_options(easy_out.serve = TRUE)

  browse_url(fs::path(first_dir, "a", ext = "html"), first_dir)
  handle <- .hebstr$.server$handle

  browse_url(fs::path(second_dir, "b", ext = "html"), second_dir)

  expect_false(identical(.hebstr$.server$handle, handle))
  expect_false(handle$isRunning())
  expect_identical(.hebstr$.server$dir, fs::path_abs(second_dir))
})

test_that("browse_url() restarts the server when the port option changes", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()

  withr::local_options(easy_out.serve = TRUE)

  browse_url(fs::path(tmp, "a", ext = "html"), tmp)
  handle <- .hebstr$.server$handle

  probe <- suppressMessages(httpuv::runStaticServer(
    dir = tmp,
    host = "127.0.0.1",
    port = NULL,
    background = TRUE,
    browse = FALSE
  ))
  wanted <- probe$getPort()
  httpuv::stopServer(probe)

  withr::local_options(easy_out.port = wanted)

  url <- browse_url(fs::path(tmp, "b", ext = "html"), tmp)

  expect_false(identical(.hebstr$.server$handle, handle))
  expect_false(handle$isRunning())
  expect_equal(.hebstr$.server$handle$getPort(), wanted)
  expect_match(url, paste0("^http://localhost:", wanted, "/"))
})

test_that("browse_url() falls back to the file path when the server cannot start", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()
  target <- fs::path(tmp, "report", ext = "html")

  blocker <- suppressMessages(httpuv::runStaticServer(
    dir = tmp,
    host = "127.0.0.1",
    port = NULL,
    background = TRUE,
    browse = FALSE
  ))
  withr::defer(httpuv::stopServer(blocker))

  withr::local_options(
    easy_out.serve = TRUE,
    easy_out.port = blocker$getPort()
  )

  expect_identical(suppressMessages(browse_url(target, tmp)), target)
  expect_null(.hebstr$.server)
})

test_that("browse_remote() reads SSH_CONNECTION when the option is unset", {
  withr::local_options(easy_out.serve = NULL)

  withr::with_envvar(
    c(SSH_CONNECTION = "10.0.0.1 54321 10.0.0.2 22"),
    expect_true(browse_remote())
  )

  withr::with_envvar(
    c(SSH_CONNECTION = ""),
    expect_false(browse_remote())
  )
})

test_that("browse_remote() rejects a non-boolean easy_out.serve", {
  withr::local_envvar(SSH_CONNECTION = "")

  withr::local_options(easy_out.serve = "yes")
  expect_error(
    browse_remote(),
    class = "rlang_error",
    regexp = "easy_out.serve"
  )

  withr::local_options(easy_out.serve = NA)
  expect_error(
    browse_remote(),
    class = "rlang_error",
    regexp = "easy_out.serve"
  )
})

test_that("browse_remote() lets the option override the environment", {
  withr::local_envvar(SSH_CONNECTION = "10.0.0.1 54321 10.0.0.2 22")

  withr::local_options(easy_out.serve = FALSE)
  expect_false(browse_remote())
})

test_that("easy_out() opens a served URL in a remote session", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  local_server()
  wb <- get_xlsx(list(a = head(iris, 3)))

  opened <- NULL
  local_mocked_bindings(
    browseURL = \(url, ...) {
      opened <<- url
      invisible(NULL)
    }
  )

  withr::local_options(easy_out.serve = TRUE)

  easy_out(wb, filename = "book", dir = tmp, quiet = FALSE)

  expect_match(opened, "^http://localhost:[0-9]+/book\\.xlsx$")
})

test_that("easy_out() opens the file path outside a remote session", {
  tmp <- withr::local_tempdir()
  local_server()
  wb <- get_xlsx(list(a = head(iris, 3)))

  opened <- NULL
  local_mocked_bindings(
    browseURL = \(url, ...) {
      opened <<- url
      invisible(NULL)
    }
  )

  withr::local_options(easy_out.serve = FALSE)

  easy_out(wb, filename = "book", dir = tmp, quiet = FALSE)

  expect_identical(opened, fs::path(tmp, "book", ext = "xlsx"))
  expect_null(.hebstr$.server)
})

test_that("local_server() clears a server left by an earlier call", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  withr::defer(browse_stop())

  withr::local_options(easy_out.serve = TRUE)

  browse_url(fs::path(tmp, "a", ext = "html"), tmp)
  expect_false(is.null(.hebstr$.server))

  local_server()

  expect_null(.hebstr$.server)
})

test_that("local_server() leaves no handle behind on exit", {
  skip_on_cran()

  tmp <- withr::local_tempdir()
  withr::defer(browse_stop())

  withr::local_options(easy_out.serve = TRUE)

  browse_url(fs::path(tmp, "a", ext = "html"), tmp)

  local({
    local_server()
    browse_url(fs::path(tmp, "b", ext = "html"), tmp)
  })

  expect_null(.hebstr$.server)
})

### WITH_FIG_DEVICE --------------------------------------------------------------

test_that("with_fig_device() evaluates code on a device of the requested size", {
  expect_equal(
    with_fig_device(width = 11.5, height = 8, code = grDevices::dev.size("in")),
    c(11.5, 8)
  )
})

test_that("with_fig_device() returns the value of code", {
  expect_identical(
    with_fig_device(width = 4, height = 3, code = "value"),
    "value"
  )
})

test_that("with_fig_device() shadows the ambient device for unit conversion", {
  .local_device(width = 7, height = 5)

  npc_in_inches <- \() {
    grid::convertHeight(grid::unit(1, "npc"), "in", valueOnly = TRUE)
  }

  expect_equal(npc_in_inches(), 5)
  expect_equal(
    with_fig_device(width = 11.5, height = 8, code = npc_in_inches()),
    8
  )
  expect_equal(npc_in_inches(), 5)
})

test_that("with_fig_device() restores the device that was current", {
  ambient <- .local_device(width = 7, height = 5)

  with_fig_device(width = 11.5, height = 8, code = NULL)

  expect_identical(grDevices::dev.cur(), ambient)
  expect_length(grDevices::dev.list(), 1L)
})

test_that("with_fig_device() closes its own device when code errors after opening another", {
  .local_device(width = 7, height = 5)
  before <- grDevices::dev.list()

  expect_error(
    with_fig_device(
      width = 11.5,
      height = 8,
      code = {
        svglite::svgstring(width = 3, height = 3)
        stop("failure inside the block")
      }
    ),
    "failure inside the block"
  )

  leaked <- setdiff(grDevices::dev.list(), before)

  expect_length(leaked, 1L)

  grDevices::dev.set(leaked)
  expect_equal(grDevices::dev.size("in"), c(3, 3))
  grDevices::dev.off(leaked)
})

test_that("with_fig_device() writes no file", {
  dir <- withr::local_tempdir()
  withr::local_dir(dir)

  with_fig_device(width = 11.5, height = 8, code = NULL)

  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("with_fig_device() rejects a size svglite would silently accept", {
  expect_error(
    with_fig_device(width = -1, height = 8, code = NULL),
    "width.*positive"
  )
  expect_error(
    with_fig_device(width = 11.5, height = NA, code = NULL),
    "height.*positive"
  )
  expect_error(
    with_fig_device(width = c(7, 8), height = 8, code = NULL),
    "width.*single positive"
  )
  expect_length(grDevices::dev.list(), 0L)
})
