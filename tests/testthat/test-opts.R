test_that("set_opts(.assign = FALSE) returns the options with a custom .name", {
  res <- set_opts(.assign = FALSE, .name = "custom_opts_regression")

  expect_type(res, "list")
  expect_contains(names(res), c("parametric", "qt_stat", "font"))
  expect_false(exists("custom_opts_regression", envir = .hebstr))
})

test_that("set_opts(.assign = FALSE) returns the options with the default .name", {
  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_type(res, "list")
  expect_contains(names(res), c("parametric", "qt_stat", "font"))
})

test_that("set_opts() accepts unknown names in ... as user-defined extensions", {
  res <- set_opts(.assign = FALSE, note = list(ajust = "adjusted on X"))

  expect_equal(res$note, list(ajust = "adjusted on X"))
})

test_that("set_opts(.assign = FALSE) applies ... overrides over an existing object", {
  local_hebstr("opts", list(sentinel = TRUE))

  res <- set_opts(.assign = FALSE, sep = list(int = " >> "))

  expect_null(res$sentinel)
  expect_equal(res$sep$int, " >> ")
  expect_equal(res$sep$ext, "; ")
})

test_that("set_opts(.assign = FALSE) recomputes when .default_font is supplied", {
  local_hebstr("opts", list(sentinel = TRUE))

  res <- set_opts(.assign = FALSE, .default_font = "arial")

  expect_null(res$sentinel)
  expect_contains(names(res), "font")
})

test_that("set_opts(.assign = FALSE) returns the existing object when no override is supplied", {
  local_hebstr("opts", list(sentinel = TRUE))

  res <- set_opts(.assign = FALSE)

  expect_true(res$sentinel)
})

test_that("set_opts() keeps font as a named alpha/digit list when both are equal", {
  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_type(res$font, "list")
  expect_named(res$font, c("alpha", "digit"))
})

test_that("set_opts() normalizes a scalar font override to a named alpha/digit list", {
  res <- set_opts(.assign = FALSE, font = "Courier")

  expect_named(res$font, c("alpha", "digit"))
  expect_identical(res$font$alpha, res$font$digit)
})

test_that("set_opts() applies French labels when OutDec is a comma", {
  withr::local_options(OutDec = ",")
  local_hebstr("opts")

  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_equal(res$labs$header, "Variable")
  expect_equal(res$labs$overall, "Total")
  expect_equal(res$labs$sex$m, "Hommes")
  expect_equal(res$labs$bin$yes, "Oui")
  expect_equal(res$sep$int, " : ")
  expect_match(as.character(res$ci$label), "IC95%")
  expect_match(names(res$qt_stat$median), "Médiane")
})

test_that("set_opts() uses English labels by default", {
  withr::local_options(OutDec = ".")
  local_hebstr("opts")

  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_equal(res$labs$header, "Characteristic")
  expect_equal(res$labs$overall, "Overall")
  expect_equal(res$sep$int, ": ")
  expect_match(as.character(res$ci$label), "95%CI")
})

# Characterization / pinning tests: lock the observable behavior the idiomatic
# refactor of set_opts() touches (deferred opts$vars formulas, qt_stat_wide, ci)
# before restructuring the builder. Fresh builds forced via explicit .default_font.

test_that("set_opts() builds opts$vars as deferred test/stat/label formula lists", {
  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_named(res$vars, c("test", "stat", "label"))
  expect_length(res$vars$test, 3)
  expect_s3_class(res$vars$test[[1]], "formula")
})

test_that("opts$vars formulas resolve against an explicit .vars_envir", {
  fake <- list(
    qt = list(
      vars = list(parametric = "mpg", nonparametric = "wt"),
      stat = list(mean = c(Mean = "{mean}"), median = c(Med = "{median}"))
    ),
    ql = list(stat = list(n = c(n = "{n}")))
  )

  res <- set_opts(.assign = FALSE, .vars_envir = fake)
  f <- res$vars$test[[1]]

  expect_equal(eval(rlang::f_lhs(f), environment(f)), "mpg")
})

test_that("opts$vars formulas abort when no vars context is available", {
  local_vars_context()
  rm_vars_context()

  res <- set_opts(.assign = FALSE, .default_font = "sans")
  f <- res$vars$test[[1]]

  expect_error(eval(rlang::f_lhs(f), environment(f)), "vars_context")
})

test_that("opts$vars stays resolvable after clear_vars between two formula evals", {
  gl <- .hebstr
  local_vars_context()
  fake <- list(
    qt = list(
      vars = list(parametric = "mpg", nonparametric = "wt"),
      stat = list(mean = c(Mean = "{mean}"), median = c(Med = "{median}"))
    ),
    ql = list(stat = list(n = c(n = "{n}")))
  )
  assign(".vars_context", new.env(parent = emptyenv()), envir = gl)
  gl$.vars_context$current <- fake

  res <- set_opts(.assign = FALSE, .default_font = "sans")
  f1 <- res$vars$test[[1]]
  f2 <- res$vars$test[[2]]

  expect_equal(eval(rlang::f_lhs(f1), environment(f1)), "mpg")
  rm(list = ".vars_context", envir = gl)
  expect_equal(eval(rlang::f_lhs(f2), environment(f2)), "wt")
})

test_that("use_vars() caches the classification in the internal store", {
  local_opts()
  local_vars_context()

  out <- use_vars(mtcars)

  expect_identical(out, mtcars)
  expect_true(exists(".vars_context", envir = .hebstr, inherits = FALSE))
  expect_false(is.null(.hebstr$.vars_context$current))
})

test_that("clear_vars() removes the cache from the internal store", {
  local_vars_context()
  .hebstr$.vars_context <- new.env(parent = emptyenv())

  clear_vars()

  expect_false(exists(".vars_context", envir = .hebstr, inherits = FALSE))
})

test_that("clear_vars() is a no-op when no cache is present", {
  local_vars_context()
  rm_vars_context()

  expect_no_error(clear_vars())
})

test_that("set_opts() strips the IQR suffix in opts$qt_stat_wide", {
  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_type(res$qt_stat_wide, "character")
  expect_contains(names(res$qt_stat_wide), "Median")
  expect_equal(unname(res$qt_stat_wide[["Median"]]), "{median}")
})

test_that("set_opts() formats opts$ci$data as a bracketed glue template", {
  withr::local_options(OutDec = ".")

  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_equal(as.character(res$ci$data), "[{conf.low}; {conf.high}]")
})

test_that("check_opts() resolves keys of the global opts object", {
  local_hebstr("opts", list(sep = list(int = ": ", ext = "; ")))

  expect_equal(check_opts(sep$int), ": ")
})

test_that("check_opts() aborts on an absent key instead of falling through to scope", {
  local_hebstr("opts", list(parametric = "x"))
  local_hebstr("phantom_key", "leaked")

  expect_error(check_opts(phantom_key), "phantom_key")
})

test_that("check_opts() aborts on an absent root key of a nested expression", {
  local_hebstr("opts", list(parametric = "x"))
  local_hebstr("color", list(cold = c("#FFF", "#000")))

  expect_error(check_opts(color$cold[1]), "color")
})

test_that("check_opts() aborts when the opts object does not exist", {
  local_hebstr("opts")

  expect_error(check_opts(font), "does not exist")
})

test_that("get_opts() returns the whole options object from the internal store", {
  local_hebstr("opts", list(sep = list(int = ": "), font = "x"))

  res <- get_opts()

  expect_type(res, "list")
  expect_equal(res$sep$int, ": ")
})

test_that("get_opts() reads a custom .name from the internal store", {
  local_hebstr("profile_opts", list(sentinel = TRUE))

  expect_true(get_opts(.name = "profile_opts")$sentinel)
})

test_that("get_opts() aborts when the options object does not exist", {
  local_hebstr("opts")

  expect_error(get_opts(), "does not exist")
})

test_that("lang_fr() sets French locale", {
  withr::local_options(OutDec = ".")

  local_mocked_bindings(
    theme_gtsummary_language = \(...) invisible(NULL)
  )

  lang_fr()

  expect_equal(getOption("OutDec"), ",")
})

test_that("lang_fr(reset = TRUE) restores English defaults", {
  withr::local_options(OutDec = ",")

  local_mocked_bindings(
    reset_gtsummary_theme = \(...) invisible(NULL)
  )

  lang_fr(reset = TRUE)

  expect_equal(getOption("OutDec"), ".")
})

test_that("lang_fr() emits FR message", {
  withr::local_options(OutDec = ".")

  local_mocked_bindings(
    theme_gtsummary_language = \(...) invisible(NULL)
  )

  expect_message(lang_fr(), "FR")
})

test_that("lang_fr(reset = TRUE) emits EN message", {
  withr::local_options(OutDec = ",")

  local_mocked_bindings(
    reset_gtsummary_theme = \(...) invisible(NULL)
  )

  expect_message(lang_fr(reset = TRUE), "EN")
})

test_that("lang_fr() calls theme_gtsummary_language with correct args", {
  withr::local_options(OutDec = ".")

  called_with <- list()
  local_mocked_bindings(
    theme_gtsummary_language = \(language, big.mark, ...) {
      called_with <<- list(language = language, big.mark = big.mark)
      invisible(NULL)
    }
  )

  lang_fr()

  expect_equal(called_with$language, "fr")
  expect_equal(called_with$big.mark, " ")
})

test_that("lang_fr(reset = TRUE) calls reset_gtsummary_theme", {
  withr::local_options(OutDec = ",")

  reset_called <- FALSE
  local_mocked_bindings(
    reset_gtsummary_theme = \(...) {
      reset_called <<- TRUE
      invisible(NULL)
    }
  )

  lang_fr(reset = TRUE)

  expect_true(reset_called)
})

test_that("set_opts() carries a page_width in inches", {
  res <- set_opts(.assign = FALSE, .default_font = "sans")

  expect_equal(res$page_width, 6.5)
})

test_that("docx_page_width() measures the usable text width of a template", {
  path <- withr::local_tempfile(fileext = ".docx")

  officer::read_docx() |>
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(width = 8.5, height = 11),
        page_margins = officer::page_mar(left = 1, right = 1)
      )
    ) |>
    print(target = path)

  expect_equal(docx_page_width(path), 6.5)
})

test_that("docx_page_width() measures a landscape template on its long edge", {
  path <- withr::local_tempfile(fileext = ".docx")

  officer::read_docx() |>
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(
          width = 8.5,
          height = 11,
          orient = "landscape"
        ),
        page_margins = officer::page_mar(left = 1, right = 1)
      )
    ) |>
    print(target = path)

  expect_equal(docx_page_width(path), 9)
})

test_that("docx_page_width() aborts on a path that does not exist", {
  expect_error(docx_page_width("no-such-template.dotx"), "does not exist")
})

test_that("docx_page_width() discovers the template of an installed extension", {
  root <- withr::local_tempdir()
  .make_extension(root)

  doc <- fs::path(root, "sub")
  fs::dir_create(doc)
  withr::local_envvar(QUARTO_DOCUMENT_PATH = doc)

  expect_equal(docx_page_width(), 6.5)
})

test_that("docx_page_width() prefers an explicit path over discovery", {
  root <- withr::local_tempdir()
  .make_extension(root)
  withr::local_envvar(QUARTO_DOCUMENT_PATH = root)

  path <- withr::local_tempfile(fileext = ".docx")

  officer::read_docx() |>
    officer::body_set_default_section(
      officer::prop_section(
        page_size = officer::page_size(width = 8.5, height = 11),
        page_margins = officer::page_mar(left = 2, right = 2)
      )
    ) |>
    print(target = path)

  expect_equal(docx_page_width(path), 4.5)
})

test_that("docx_page_width() aborts when no extension declares a template", {
  root <- withr::local_tempdir()
  .make_extension(root, reference_doc = NULL)
  withr::local_envvar(QUARTO_DOCUMENT_PATH = root)

  expect_error(docx_page_width(), "0 Word templates found")
})

test_that("docx_page_width() aborts when several extensions declare a template", {
  root <- withr::local_tempdir()
  .make_extension(root, id = "org/one")
  .make_extension(root, id = "org/two")
  withr::local_envvar(QUARTO_DOCUMENT_PATH = root)

  expect_error(docx_page_width(), "2 Word templates found")
})
