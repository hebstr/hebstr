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

test_that("theme_gt() justifies the title by default, as it does the footnotes", {
  local_opts()

  themed <- theme_gt(.make_gt_titled())

  expect_identical(.gt_align(themed, "title"), "justify")
  expect_identical(.gt_align(themed, "footnotes"), "justify")
})

test_that("theme_gt(title_align) reaches the title, which the refinements used to override", {
  local_opts()

  themed <- theme_gt(.make_gt_titled(), title_align = "center")

  expect_identical(.gt_align(themed, "title"), "center")
  expect_identical(.gt_align(themed, "footnotes"), "justify")
})

test_that("tbl_format(title_align) carries through to the title", {
  local_opts()

  themed <- tbl_format(.make_summary_tbl(), title = "T", title_align = "center")

  expect_identical(.gt_align(themed, "title"), "center")
})

test_that("theme_gt(title_align) reaches the heading under docx, which drops the cell refinement", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  themed <- theme_gt(.make_gt_titled(), title_align = "center")

  expect_identical(.gt_heading_align(themed), "center")
  expect_equal(nrow(themed$`_styles`), 0)
})

test_that("theme_gt() carries its justify default to the heading under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  themed <- theme_gt(.make_gt_titled())

  expect_identical(.gt_heading_align(themed), "justify")
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
  expect_match(digit_fonts, "^DigitFace,", all = FALSE)
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

  themed <- theme_ft(
    flextable::flextable(data.frame(label = "a", stat = "1.0")),
    alpha = "AlphaFace",
    digit = "DigitFace"
  )

  fonts <- themed$body$styles$text$font.family$data

  expect_identical(unique(fonts[, "label"]), "AlphaFace")
  expect_identical(unique(fonts[, "stat"]), "DigitFace")
})

test_that("theme_ft() sets a Word table in Aptos whatever the session font", {
  local_opts()
  .hebstr$opts$font <- list(alpha = "AlphaFace", digit = "DigitFace")

  ft <- flextable::flextable(data.frame(label = "a", stat = "1.0"))

  expect_identical(
    unique(as.vector(theme_ft(ft)$body$styles$text$font.family$data)),
    "Aptos"
  )
  expect_identical(
    unique(as.vector(
      theme_ft(ft, alpha = "AlphaFace")$body$styles$text$font.family$data
    )),
    "AlphaFace"
  )
})

test_that("theme_ft() tints the first body row, as the gt striping does", {
  local_opts()

  band <- check_opts(color$cold[1])
  bg <- theme_ft(.make_ft_mtcars())$body$styles$cell$background.color$data

  expect_identical(unique(bg[1, ]), band)
  expect_identical(unique(bg[2, ]), "white")
})

test_that("theme_ft() styles a table with no body row", {
  local_opts()

  expect_no_error(theme_ft(flextable::flextable(head(mtcars, 0))))
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

test_that("theme_rt() returns a reactableTheme carrying the widget palette", {
  theme <- theme_rt()

  expect_s3_class(theme, "reactableTheme")
  expect_identical(theme$style$fontSize, .rt_size)
  expect_identical(theme$backgroundColor, "var(--primary-surface, #ffffff)")
  expect_match(theme$stripedColor, "^var\\(--primary-back, #")
  expect_match(theme$borderColor, "^color-mix\\(")
})

test_that("theme_rt() falls each token back to the package palette", {
  theme <- theme_rt()

  expect_identical(
    theme$stripedColor,
    paste0("var(--primary-back, ", set_opts(.assign = FALSE)$color$cold[1], ")")
  )
})

test_that("theme_rt() colors the expander arrow, which no theme argument reaches", {
  theme <- theme_rt(expander_color = "#123456")

  expect_identical(
    theme$style[[".rt-expander:after"]],
    list(borderTopColor = "#123456")
  )
})

test_that("theme_rt() renders standalone when opts is absent", {
  local_hebstr("opts")

  expect_match(theme_rt()$style$fontFamily, "^sans,")
})

test_that("theme_rt() reads the centralised text font", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  expect_match(theme_rt()$style$fontFamily, "^PinnedAlpha,")
})

test_that("theme_rt() forwards its dots to reactableTheme()", {
  theme <- theme_rt(strip_color = "#fff", highlightColor = "#eeeeee")

  expect_identical(theme$highlightColor, "#eeeeee")
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
  # the registry is neutralised too: .onLoad() declares the shipped faces to
  # it, so the absence being simulated here has to cover both sources
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    registry_fonts = \() data.frame(family = character(0)),
    .package = "systemfonts"
  )

  expect_identical(check_fonts(.auto = "luciole"), "sans")
})

test_that("the package declares its shipped faces to systemfonts", {
  registered <- systemfonts::registry_fonts()

  # the registry folds a family to one lowercase name whatever it was declared
  # under, so this is the only spelling it can be asked for
  expect_contains(registered$family, names(.docx_faces))

  # the registry wins over any system install of the same family, so the file
  # served is the one the package ships. Named rather than rederived from
  # .font_faces(), which .onLoad() fed the registry with: the two would move
  # together and a bold face declared as the regular one would read as correct
  expect_identical(
    fs::path_file(systemfonts::match_fonts("luciole")$path),
    "Luciole-Regular.ttf"
  )
})

test_that(".font_faces() fills a slot with the standard face, not a sibling cut", {
  weights <- c(
    "thin",
    "ultralight",
    "light",
    "normal",
    "medium",
    "semibold",
    "bold",
    "ultrabold",
    "heavy"
  )

  # the shape a real family takes: an optical-size cut and a black cut both
  # report weight normal, and a semibold sits between normal and bold
  fonts <- data.frame(
    family = "Fake Aptos",
    style = c("Display", "Black", "Regular", "SemiBold", "Bold", "Italic"),
    weight = factor(
      c("normal", "normal", "normal", "semibold", "bold", "normal"),
      levels = weights,
      ordered = TRUE
    ),
    italic = c(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE),
    width = "normal",
    path = c(
      "display.ttf",
      "black.ttf",
      "regular.ttf",
      "semibold.ttf",
      "bold.ttf",
      "italic.ttf"
    )
  )

  local_mocked_bindings(system_fonts = \() fonts, .package = "systemfonts")

  faces <- .font_faces("Fake Aptos")

  expect_identical(faces$regular, "regular.ttf")
  expect_identical(faces$bold, "bold.ttf")
  expect_identical(faces$italic, "italic.ttf")
})

test_that(".font_faces() leaves a slot empty rather than duplicate a face", {
  weights <- c(
    "thin",
    "ultralight",
    "light",
    "normal",
    "medium",
    "semibold",
    "bold",
    "ultrabold",
    "heavy"
  )

  # a family shipping the upright pair alone, and a variable font reporting
  # one file under two styles: both would hand Word an embedded face it is
  # better off synthesising
  fonts <- data.frame(
    family = "Fake Thin",
    style = c("Regular", "Italic", "Bold"),
    weight = factor(
      c("normal", "normal", "bold"),
      levels = weights,
      ordered = TRUE
    ),
    italic = c(FALSE, TRUE, FALSE),
    width = "normal",
    path = c("one.ttf", "one-italic.ttf", "one.ttf")
  )

  local_mocked_bindings(system_fonts = \() fonts, .package = "systemfonts")

  faces <- .font_faces("Fake Thin")

  expect_named(faces, c("regular", "italic"))
})

test_that("theme_gt() carries a CSS fallback after the family", {
  local_opts()

  themed <- theme_gt(gt::gt(head(mtcars, 2)))

  stack <- themed$`_options`$value[[
    which(themed$`_options`$parameter == "table_font_names")
  ]]

  # a bare family is what leaves a reader without it on Times New Roman
  expect_gt(length(stack), 1L)
  expect_identical(stack[[length(stack)]], "sans-serif")
})

test_that("check_fonts() reports a font registered from bundled files", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = c("Fake Sans", "Fake Mono")),
    registry_fonts = \() data.frame(family = "Fake Registered"),
    .package = "systemfonts"
  )

  expect_true(check_fonts("Fake Registered"))
  expect_identical(check_fonts(.auto = "Fake Registered"), "Fake Registered")
})

test_that("check_fonts() falls back when the registry holds no match either", {
  local_mocked_bindings(
    system_fonts = \() data.frame(family = "Fake Sans"),
    registry_fonts = \() data.frame(family = character(0)),
    .package = "systemfonts"
  )

  expect_false(check_fonts("Zzz Not Installed"))
  expect_identical(check_fonts(.auto = "Zzz Not Installed"), "sans")
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
