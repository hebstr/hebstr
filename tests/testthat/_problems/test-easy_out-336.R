# Extracted from test-easy_out.R:336

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
easy_out(
  p,
  filename = "fig",
  suffix = "final",
  sep = "-",
  dir = tmp,
  quiet = TRUE
)
