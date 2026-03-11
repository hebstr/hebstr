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

  local_mocked_bindings(
    capturePlot = \(...) "mock_path.svg",
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  expect_no_error(
    easy_out(p, filename = "test_plot", dir = tmp, quiet = TRUE)
  )

  expect_true(fs::dir_exists(tmp))
})

test_that("easy_out() builds filename with suffix", {
  skip_if_not_installed("ggplot2")

  tmp <- withr::local_tempdir()
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(
    capturePlot = \(...) "mock_path.svg",
    image_read_svg = \(...) "mock_img",
    image_write = \(...) invisible(NULL),
    browseURL = \(...) invisible(NULL)
  )

  expect_no_error(
    easy_out(
      p,
      filename = "myplot",
      suffix = "v2",
      dir = tmp,
      quiet = TRUE
    )
  )
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
    is_phantomjs_installed = \() TRUE,
    .package = "webshot"
  )

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

  expect_true(fs::dir_exists(tmp))
  expect_true(grepl("test_table\\.html$", saved_html))
  expect_true(webshot_called)
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
    sep = "_",
    dir = tmp,
    quiet = TRUE
  )

  expect_true(grepl("fig_final", captured_svg))
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
