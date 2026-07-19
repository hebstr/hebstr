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

test_that("easy_out() rejects a non-boolean export", {
  expect_error(
    easy_out(mtcars, export = "yes", quiet = TRUE),
    class = "rlang_error"
  )
})

test_that("easy_out() writes nothing when export is FALSE", {
  skip_if_not_installed("ggplot2")

  tmp <- withr::local_tempdir()
  dir <- fs::path(tmp, "skipped")
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  expect_null(easy_out(p, filename = "test", dir = dir, export = FALSE))
  expect_false(fs::dir_exists(dir))
})

test_that("easy_out() leaves the variable context intact when export is FALSE", {
  skip_if_not_installed("ggplot2")

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

test_that("easy_out.export option overrides the hebstr.docx default", {
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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

test_that("easy_out() builds filename with suffix", {
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("gt")

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
  skip_if_not_installed("gt")

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
  expect_match(applied_width, "700")
})

test_that("easy_out() uses user-supplied width when gt has no table_width", {
  skip_if_not_installed("gt")

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
  skip_if_not_installed("gt")

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

test_that("easy_out() ignores a percentage table_width and falls back to default px", {
  skip_if_not_installed("gt")

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
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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

test_that("easy_out() accepts a gtsummary object and calls as_gt", {
  skip_if_not_installed("gt")
  skip_if_not_installed("gtsummary")

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
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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

test_that("easy_out_map() rejects an unnamed list", {
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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
  skip_if_not_installed("ggplot2")

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
    class = "rlang_error"
  )
})

test_that("svg_crop() trims the canvas to the drawing", {
  path <- .make_svg(
    grid::rectGrob(x = 0.4, y = 0.6, width = 0.3, height = 0.2)
  )

  before <- .view_box(path)

  expect_true(svg_crop(path))

  after <- .view_box(path)

  expect_equal(after[3], 0.30 * before[3], tolerance = 0.1)
  expect_equal(after[4], 0.20 * before[4], tolerance = 0.2)
  expect_gt(after[1], before[1])
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

  header <- grep("<svg[ >]", readLines(path), value = TRUE)
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
    0.30 * 9 * 72,
    tolerance = 0.1
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
  skip_if_not_installed("ggplot2")

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
