# Extracted from test-easy_out.R:406

# test -------------------------------------------------------------------------
skip_if_not_installed("ggplot2")
tmp <- withr::local_tempdir()
custom_dir <- fs::path(tmp, "custom_output")
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
  ggplot2::geom_point()
local_mocked_bindings(
  ggsave = \(...) "mock_path.svg",
  image_read_svg = \(...) "mock_img",
  image_write = \(...) invisible(NULL),
  browseURL = \(...) invisible(NULL)
)
withr::local_options(easy_out.dir = custom_dir)
easy_out(p, filename = "test_dir", quiet = TRUE)
