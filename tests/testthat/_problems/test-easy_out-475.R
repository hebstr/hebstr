# Extracted from test-easy_out.R:475

# test -------------------------------------------------------------------------
tmp <- withr::local_tempdir()
g <- grid::grobTree(grid::rectGrob(), grid::textGrob("a    b"))
local_mocked_bindings(
  image_read_svg = \(...) "mock_img",
  image_write = \(...) invisible(NULL),
  browseURL = \(...) invisible(NULL)
)
easy_out(g, filename = "test_space", dir = tmp, quiet = TRUE)
svg <- readLines(fs::path(tmp, "test_space", ext = "svg"))
expect_match(
  paste(svg, collapse = "\n"),
  'xml:space="preserve"',
  fixed = TRUE
)
