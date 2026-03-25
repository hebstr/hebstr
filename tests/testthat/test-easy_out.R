test_that("easy_out() rejects unsupported objects", {
  expect_error(
    easy_out(mtcars, quiet = TRUE),
    class = "rlang_error"
  )
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
    capturePlot = \(...) "mock_path.svg",
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
    capturePlot = \(expr, filename, ...) {
      captured_svg <<- filename
      "mock_path.svg"
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
    capturePlot = \(...) "mock_path.svg",
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
    capturePlot = \(expr, filename, ...) {
      captured_svg <<- filename
      "mock_path.svg"
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
    capturePlot = \(...) "mock_path.svg",
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
    capturePlot = \(...) "mock_path.svg",
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
    capturePlot = \(...) "mock_path.svg",
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  withr::local_options(easy_out.dir = option_dir)

  easy_out(p, filename = "test_dir", dir = arg_dir, quiet = TRUE)

  expect_true(fs::dir_exists(arg_dir))
  expect_false(fs::dir_exists(option_dir))
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
