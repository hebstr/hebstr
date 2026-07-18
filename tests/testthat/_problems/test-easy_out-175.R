# Extracted from test-easy_out.R:175

# test -------------------------------------------------------------------------
skip_if_not_installed("ggplot2")
tmp <- withr::local_tempdir()
output_dir <- fs::path(tmp, "new_folder")
p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
  ggplot2::geom_point()
local_mocked_bindings(
  ggsave = \(...) "mock_path.svg",
  image_read_svg = \(...) "mock_img",
  image_write = \(...) invisible(NULL),
  browseURL = \(...) invisible(NULL)
)
easy_out(p, filename = "test", dir = output_dir, quiet = TRUE)
