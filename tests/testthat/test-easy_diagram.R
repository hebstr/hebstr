### CONSTRUCTOR -----------------------------------------------------------------

test_that("easy_diagram() keeps the text under its own class", {
  d <- easy_diagram("erDiagram\n  A }o--|| B : \"\"")

  expect_s3_class(d, "hebstr_diagram")
  expect_identical(d$text, "erDiagram\n  A }o--|| B : \"\"")
})

test_that("easy_diagram() rejects anything but a single non-blank string", {
  bad <- list(1, NA_character_, "", "  \n ", c("erDiagram", "  A"), NULL, list("erDiagram"))

  for (x in bad) {
    expect_error(easy_diagram(x), class = "rlang_error", regexp = "`text` must be")
  }
})

test_that("easy_diagram() needs no Node: rendering is left to the export", {
  withr::local_envvar(PATH = "/nonexistent")

  expect_s3_class(easy_diagram("erDiagram\n  A"), "hebstr_diagram")
})

### EASY_OUT GUARDS -------------------------------------------------------------

test_that("easy_out() accepts a diagram without writing under export = FALSE", {
  withr::local_envvar(PATH = "/nonexistent")

  tmp <- withr::local_tempdir()
  out <- fs::path(tmp, "out")
  d <- easy_diagram("erDiagram\n  A")

  expect_null(easy_out(d, dir = out, export = FALSE, quiet = TRUE))
  expect_false(fs::dir_exists(out))
})

test_that("easy_out() refuses a slide of a diagram, naming its class", {
  d <- easy_diagram("erDiagram\n  A")

  expect_error(
    easy_out(d, pptx = TRUE, export = FALSE, quiet = TRUE),
    class = "rlang_error",
    regexp = "only covers a plot or a grid grob"
  )
})

test_that("easy_out() names the diagram among the classes it supports", {
  expect_error(
    easy_out(mtcars, quiet = TRUE),
    regexp = "diagram",
    class = "rlang_error"
  )
})

### NODE ------------------------------------------------------------------------

test_that("easy_out() aborts on a missing Node before creating any folder", {
  withr::local_envvar(PATH = "/nonexistent")

  out <- fs::path(withr::local_tempdir(), "out")
  d <- easy_diagram("erDiagram\n  A")

  expect_error(
    easy_out(d, dir = out, quiet = TRUE),
    class = "rlang_error",
    regexp = "needs Node.js"
  )
  expect_false(fs::dir_exists(out))
})

test_that("easy_out() clears a list for Node before writing its first element", {
  withr::local_envvar(PATH = "/nonexistent")

  out <- fs::path(withr::local_tempdir(), "out")
  d <- easy_diagram("erDiagram\n  A")

  expect_error(
    easy_out(
      list(fig = grid::rectGrob(), er = d),
      filename = "fig_mixed",
      dir = out,
      quiet = TRUE
    ),
    class = "rlang_error",
    regexp = "needs Node.js"
  )
  expect_false(fs::dir_exists(out))
})

test_that("easy_out() refuses a Node older than the Pele floor", {
  withr::local_envvar(PATH = .make_fake_node("v18.19.0"))

  d <- easy_diagram("erDiagram\n  A")

  expect_error(
    easy_out(d, dir = withr::local_tempdir(), quiet = TRUE),
    class = "rlang_error",
    regexp = "20 or later"
  )
})

### EXPORT ----------------------------------------------------------------------

test_that("easy_out() leaves no folder behind when Pele cannot read the text", {
  skip_if_not(.diagram_node_available())

  out <- fs::path(withr::local_tempdir(), "out")
  d <- easy_diagram("not a diagram at all")

  expect_error(
    easy_out(d, dir = out, quiet = TRUE),
    class = "rlang_error",
    regexp = "Pele could not render"
  )
  expect_false(fs::dir_exists(out))
})

test_that("easy_out() writes a diagram as an SVG carrying its face and palette", {
  skip_if_not(.diagram_node_available())

  local_opts(font = "luciole")

  tmp <- withr::local_tempdir()
  d <- easy_diagram("erDiagram\n  A }o--|| B : \"\"")

  seen <- env()

  local_mocked_bindings(
    webshot = \(url, file, vwidth, vheight, selector, zoom, ...) {
      seen$shot <- list(
        page = readLines(url),
        vwidth = vwidth,
        vheight = vheight,
        selector = selector
      )
      fs::file_create(file)
      invisible(file)
    }
  )

  easy_out(d, dir = tmp, quiet = TRUE)

  shot <- seen$shot

  svg <- fs::path(tmp, "d", "d.svg")
  txt <- paste(readLines(svg, warn = FALSE), collapse = "\n")

  expect_s3_class(xml2::read_xml(svg), "xml_document")
  expect_match(txt, "@font-face", fixed = TRUE)
  expect_match(txt, "font/woff2;base64,", fixed = TRUE)
  expect_match(txt, "--pele-surface:#F0FAFF;", fixed = TRUE)
  expect_match(txt, 'xml:space="preserve"', fixed = TRUE)

  expect_setequal(fs::path_file(fs::dir_ls(fs::path(tmp, "d"))), c("d.svg", "d.png"))

  expect_identical(shot$selector, "img")
  expect_match(paste(shot$page, collapse = ""), 'src="d.svg"', fixed = TRUE)

  box <- as.integer(str_extract(txt, 'width="(\\d+)" height="(\\d+)"', group = 1:2))
  expect_identical(c(shot$vwidth, shot$vheight), box)
})

test_that("easy_out() takes the palette of the session for a diagram", {
  skip_if_not(.diagram_node_available())

  local_opts(color = list(base = "#123456", cold = c("#abcdef", "#fedcba")))

  tmp <- withr::local_tempdir()
  d <- easy_diagram("erDiagram\n  A")

  local_mocked_bindings(webshot = \(url, file, ...) fs::file_create(file))

  easy_out(d, dir = tmp, quiet = TRUE)

  txt <- paste(readLines(fs::path(tmp, "d", "d.svg"), warn = FALSE), collapse = "\n")

  expect_match(txt, "--pele-surface:#abcdef;", fixed = TRUE)
  expect_match(txt, "--pele-border:#123456;", fixed = TRUE)
  expect_match(txt, "--pele-accent:#fedcba;", fixed = TRUE)
})

test_that("easy_out() names a diagram's files after the common rules", {
  skip_if_not(.diagram_node_available())

  tmp <- withr::local_tempdir()
  fig_er <- easy_diagram("erDiagram\n  A")

  local_mocked_bindings(webshot = \(url, file, ...) fs::file_create(file))

  easy_out(fig_er, suffix = "v2", dir = tmp, quiet = TRUE)

  expect_true(fs::file_exists(fs::path(tmp, "fig-er", "fig-er-v2", ext = "svg")))
  expect_true(fs::file_exists(fs::path(tmp, "fig-er", "fig-er-v2", ext = "png")))
})

test_that("easy_out() writes the diagrams of a bare list into one folder", {
  skip_if_not(.diagram_node_available())

  tmp <- withr::local_tempdir()
  panels <- list(
    pmsi = easy_diagram("erDiagram\n  A }o--|| B : \"\""),
    dcir = easy_diagram("erDiagram\n  C }o--|| D : \"\"")
  )

  local_mocked_bindings(webshot = \(url, file, ...) fs::file_create(file))

  easy_out(panels, filename = "fig_er", dir = tmp, quiet = TRUE)

  expect_setequal(
    fs::path_file(fs::dir_ls(fs::path(tmp, "fig-er"))),
    c("fig-er-pmsi.svg", "fig-er-pmsi.png", "fig-er-dcir.svg", "fig-er-dcir.png")
  )
})

test_that("easy_out() leaves no capture page behind", {
  skip_if_not(.diagram_node_available())

  tmp <- withr::local_tempdir()
  d <- easy_diagram("erDiagram\n  A")

  local_mocked_bindings(
    webshot = \(url, file, ...) {
      stop("no browser here")
    }
  )

  expect_error(easy_out(d, dir = tmp, quiet = TRUE), "no browser here")
  expect_identical(fs::path_file(fs::dir_ls(fs::path(tmp, "d"))), "d.svg")
})
