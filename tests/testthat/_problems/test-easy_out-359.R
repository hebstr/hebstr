# Extracted from test-easy_out.R:359

# test -------------------------------------------------------------------------
skip_if_not_installed("ggplot2")
tmp <- withr::local_tempdir()
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
  ggplot2::geom_point()
browse_called <- FALSE
local_mocked_bindings(
  ggsave = \(...) "mock_path.svg",
  image_read_svg = \(...) "mock_img",
  image_write = \(...) invisible(NULL),
  browseURL = \(...) {
    browse_called <<- TRUE
    invisible(NULL)
  }
)
easy_out(p, filename = "test_browse", dir = tmp, quiet = FALSE)
