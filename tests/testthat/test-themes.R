test_that("theme_gt() applies style refinements by default", {
  local_opts()

  themed <- theme_gt(.make_gt_mtcars())

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() skips style refinements when options(hebstr.docx = TRUE)", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  themed <- theme_gt(.make_gt_mtcars())

  expect_equal(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() docx argument overrides the option", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  themed <- theme_gt(.make_gt_mtcars(), docx = FALSE)

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() ignores a global object named docx", {
  local_opts()
  rlang::local_bindings(docx = TRUE, .env = globalenv())

  themed <- theme_gt(.make_gt_mtcars())

  expect_gt(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() aborts on a non-boolean docx", {
  local_opts()

  expect_error(theme_gt(.make_gt_mtcars(), docx = "yes"), "docx")
})

test_that("theme_gt() applies the alpha font to the table and the digit font to numeric cells", {
  local_opts()
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
  expect_contains(digit_fonts, "DigitFace")
})

test_that("theme_gt(row_strip = FALSE) makes the row background transparent", {
  local_opts()

  themed <- theme_gt(.make_gt_mtcars(), row_strip = FALSE)

  bg <- themed$`_options`$value[[
    which(themed$`_options`$parameter == "table_background_color")
  ]]

  expect_identical(bg, "#ffffff00")
})

test_that("theme_gt() aborts when opts does not exist (deliberately strict)", {
  local_hebstr("opts")

  expect_error(theme_gt(.make_gt_mtcars()), "does not exist")
})

test_that("theme_ft() returns a themed flextable", {
  local_opts()

  themed <- theme_ft(.make_ft_mtcars())

  expect_s3_class(themed, "flextable")
})

test_that("theme_ft() applies the alpha font to the table and the digit font to numeric cells", {
  local_opts()
  .hebstr$opts$font <- list(alpha = "AlphaFace", digit = "DigitFace")

  themed <- theme_ft(
    flextable::flextable(data.frame(label = "a", stat = "1.0"))
  )

  fonts <- themed$body$styles$text$font.family$data

  expect_identical(unique(fonts[, "label"]), "AlphaFace")
  expect_identical(unique(fonts[, "stat"]), "DigitFace")
})

test_that("theme_ft() justifies the footer, which theme_gt(docx = TRUE) has to drop", {
  local_opts()

  themed <- theme_ft(
    flextable::add_footer_lines(.make_ft_mtcars(), "a footnote")
  )

  expect_contains(
    as.vector(themed$footer$styles$par$text.align$data),
    "justify"
  )
})

test_that("theme_ft(row_strip = FALSE) drops the striping band color", {
  local_opts()

  band <- check_opts(color$cold[1])
  striped <- theme_ft(.make_ft_mtcars())
  plain <- theme_ft(.make_ft_mtcars(), row_strip = FALSE)

  expect_contains(
    as.vector(striped$body$styles$cell$background.color$data),
    band
  )
  expect_false(
    band %in% as.vector(plain$body$styles$cell$background.color$data)
  )
})

test_that("theme_ft() aborts when opts does not exist (deliberately strict)", {
  local_hebstr("opts")

  expect_error(theme_ft(.make_ft_mtcars()), "does not exist")
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

test_that("theme_bubble() adapts title and grid colors to a vector axis color", {
  themed <- theme_bubble(
    family = "",
    base_color = "#333333",
    axis_color_x = c("red", "blue")
  )

  expect_equal(themed$axis.text.x$colour, c("red", "blue"))
  expect_equal(themed$axis.title.x$colour, "#333333")
  expect_equal(
    themed$panel.grid.major.x$colour,
    colorspace::lighten(c("red", "blue"), 0.85)
  )
})

test_that("theme_bubble() accepts a transparent axis color", {
  themed <- theme_bubble(family = "", axis_color_x = NA)

  expect_s3_class(themed, "theme")
  expect_equal(
    themed$panel.grid.major.x$colour,
    colorspace::lighten(NA, 0.85)
  )
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

test_that("check_fonts() does not report an uninstalled font via word match", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Grotesk", "Fake Display")),
    .package = "systemfonts"
  )

  expect_false(check_fonts("Fake"))
  expect_false(check_fonts("Grotesk"))
  expect_true(check_fonts("Fake Grotesk"))
})

test_that("check_fonts() reports the device font aliases as available", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    .package = "systemfonts"
  )

  expect_true(check_fonts("sans"))
  expect_true(check_fonts("serif", "mono"))
  expect_identical(check_fonts(.auto = "serif"), "serif")
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
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  expect_identical(.text_font(), "PinnedAlpha")
})

test_that(".text_font() falls back to the portable 'sans' family when opts is absent", {
  local_hebstr("opts")

  expect_identical(.text_font(), "sans")
})

test_that("theme_*() default family reads opts$font$alpha", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  expect_identical(theme_bar()$text$family, "PinnedAlpha")
})
