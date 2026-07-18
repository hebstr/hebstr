# Extracted from test-easy_out.R:154

# test -------------------------------------------------------------------------
skip_if_not_installed("ggplot2")
tmp <- withr::local_tempdir()
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
  ggplot2::geom_point()
captured_svg <- NULL
local_mocked_bindings(
  ggsave = \(filename, ...) {
    captured_svg <<- filename
    "mock_path.svg"
  },
  image_read_svg = \(...) "mock_img",
  image_write = \(...) invisible(NULL),
  browseURL = \(...) invisible(NULL)
)
easy_out(p, filename = "myplot", suffix = "v2", dir = tmp, quiet = TRUE)
