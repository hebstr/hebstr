.make_gt <- \() gt::gt(head(mtcars))

test_that("theme_gt() applies style refinements by default", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  themed <- theme_gt(.make_gt())

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() skips style refinements when options(theme_gt.docx = TRUE)", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()
  withr::local_options(theme_gt.docx = TRUE)

  themed <- theme_gt(.make_gt())

  expect_equal(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() docx argument overrides the option", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()
  withr::local_options(theme_gt.docx = TRUE)

  themed <- theme_gt(.make_gt(), docx = FALSE)

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() ignores a global object named docx", {
  withr::defer({
    rm(list = "opts", envir = .hebstr)
    rm(list = "docx", envir = globalenv())
  })
  set_opts()
  assign("docx", TRUE, envir = globalenv())

  themed <- theme_gt(.make_gt())

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() aborts on a non-boolean docx", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  expect_error(theme_gt(.make_gt(), docx = "yes"), "docx")
})

test_that("theme_gt() applies the alpha font to the table and the digit font to numeric cells", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()
  .hebstr$opts$font <- list(alpha = "AlphaFace", digit = "DigitFace")

  themed <- theme_gt(gt::gt(data.frame(label = "a", stat = "1.0")))

  table_font <- themed$`_options`$value[[
    which(themed$`_options`$parameter == "table_font_names")
  ]]
  digit_fonts <- unlist(lapply(
    themed$`_styles`$styles,
    \(st) st$cell_text$font
  ))

  expect_identical(table_font[[1]], "AlphaFace")
  expect_true("DigitFace" %in% digit_fonts)
})

test_that("theme_gt() aborts when opts does not exist (deliberately strict)", {
  if (exists("opts", envir = .hebstr, inherits = FALSE)) {
    rm(list = "opts", envir = .hebstr)
  }

  expect_error(theme_gt(.make_gt()), "does not exist")
})

test_that("theme_bar() returns a theme honoring legend_position", {
  themed <- theme_bar(family = "", legend_position = "bottom")

  expect_s3_class(themed, "theme")
  expect_equal(themed$legend.position, "bottom")
})

test_that("theme_tte() returns a theme with a thin base line", {
  themed <- theme_tte(family = "")

  expect_s3_class(themed, "theme")
  expect_equal(themed$line$linewidth, 0.3)
})

test_that("theme_pca() returns a theme with no legend", {
  themed <- theme_pca(family = "")

  expect_s3_class(themed, "theme")
  expect_equal(themed$legend.position, "none")
})

test_that("theme_blank() returns a theme", {
  expect_s3_class(theme_blank(family = ""), "theme")
})

test_that("theme_blank() draws its grid without a deprecation warning", {
  expect_no_warning(theme_blank(family = "", grid = TRUE))
})

test_that("theme_infreq() returns a theme", {
  themed <- expect_no_warning(theme_infreq(family = ""))
  expect_s3_class(themed, "theme")
})

test_that("theme_bubble() returns a theme", {
  expect_s3_class(theme_bubble(family = ""), "theme")
})

test_that("theme_risktable() returns a list of theme components", {
  themed <- theme_risktable(family = "")

  expect_type(themed, "list")
  expect_true(any(map_lgl(themed, ~ inherits(., "theme"))))
})

test_that("check_fonts() checks every supplied font name, not just the first", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_true(check_fonts("Fake Sans", "Fake Mono"))
  expect_false(check_fonts("Fake Sans", "Zzz Not Installed"))
})

test_that("check_fonts() does not report an uninstalled font via substring match", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_false(check_fonts("Fake Sans Xtrabold"))
})

test_that("check_fonts() aborts naming the missing font", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_error(
    check_fonts("Fake Sans", "Zzz Not Installed", .abort = TRUE),
    "Zzz Not Installed"
  )
})

test_that("check_fonts() falls back to the OS-agnostic 'sans' family", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_identical(check_fonts(.auto = "luciole"), "sans")
})

test_that(".text_font() reads the centralised text font when opts exists", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = .hebstr)

  expect_identical(.text_font(), "PinnedAlpha")
})

test_that(".text_font() falls back to check_fonts(luciole) when opts is absent", {
  if (exists("opts", envir = .hebstr, inherits = FALSE)) {
    rm(list = "opts", envir = .hebstr)
  }

  expect_identical(.text_font(), check_fonts(.auto = "luciole"))
})

test_that(".text_font() resolves to 'sans' when opts absent and Luciole missing", {
  if (exists("opts", envir = .hebstr, inherits = FALSE)) {
    rm(list = "opts", envir = .hebstr)
  }
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_identical(.text_font(), "sans")
})

test_that("theme_*() default family reads opts$font$alpha", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = .hebstr)

  expect_identical(theme_bar()$text$family, "PinnedAlpha")
})
