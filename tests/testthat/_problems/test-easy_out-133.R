# Extracted from test-easy_out.R:133

# test -------------------------------------------------------------------------
skip_if_not_installed("ggplot2")
tmp <- withr::local_tempdir()
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
  ggplot2::geom_point()
write_called <- FALSE
local_mocked_bindings(
  ggsave = \(...) "mock_path.svg",
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
