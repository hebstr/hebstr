test_that("set_opts(.assign = FALSE) returns the options with a custom .name", {
  res <- set_opts(.assign = FALSE, .name = "custom_opts_regression")

  expect_type(res, "list")
  expect_contains(names(res), c("parametric", "qt_stat", "font"))
  expect_false(exists("custom_opts_regression", envir = globalenv()))
})

test_that("set_opts(.assign = FALSE) returns the options with the default .name", {
  res <- set_opts(.assign = FALSE)

  expect_type(res, "list")
  expect_contains(names(res), c("parametric", "qt_stat", "font"))
})

test_that("set_opts() aborts on unknown option names in ...", {
  expect_error(
    set_opts(.assign = FALSE, collor = list(base = "#000")),
    "collor"
  )
})

test_that("set_opts(.assign = FALSE) applies ... overrides over an existing object", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(sentinel = TRUE), envir = globalenv())

  res <- set_opts(.assign = FALSE, sep = list(int = " >> "))

  expect_null(res$sentinel)
  expect_equal(res$sep$int, " >> ")
  expect_equal(res$sep$ext, "; ")
})

test_that("set_opts(.assign = FALSE) recomputes when .default_font is supplied", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(sentinel = TRUE), envir = globalenv())

  res <- set_opts(.assign = FALSE, .default_font = "arial")

  expect_null(res$sentinel)
  expect_contains(names(res), "font")
})

test_that("set_opts(.assign = FALSE) returns the existing object when no override is supplied", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(sentinel = TRUE), envir = globalenv())

  res <- set_opts(.assign = FALSE)

  expect_true(res$sentinel)
})

test_that("set_opts() applies French labels when OutDec is a comma", {
  withr::local_options(OutDec = ",")
  if (exists("opts", envir = globalenv(), inherits = FALSE)) {
    rm(list = "opts", envir = globalenv())
  }

  res <- set_opts(.assign = FALSE, .default_font = "trebuchet ms")

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
  if (exists("opts", envir = globalenv(), inherits = FALSE)) {
    rm(list = "opts", envir = globalenv())
  }

  res <- set_opts(.assign = FALSE, .default_font = "trebuchet ms")

  expect_equal(res$labs$header, "Characteristic")
  expect_equal(res$labs$overall, "Overall")
  expect_equal(res$sep$int, ": ")
  expect_match(as.character(res$ci$label), "95%CI")
})

test_that("check_opts() resolves keys of the global opts object", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(sep = list(int = ": ", ext = "; ")), envir = globalenv())

  expect_equal(check_opts(sep$int), ": ")
})

test_that("check_opts() aborts on an absent key instead of falling through to scope", {
  withr::defer(rm(list = c("opts", "phantom_key"), envir = globalenv()))
  assign("opts", list(parametric = "x"), envir = globalenv())
  assign("phantom_key", "leaked", envir = globalenv())

  expect_error(check_opts(phantom_key), "phantom_key")
})

test_that("check_opts() aborts on an absent root key of a nested expression", {
  withr::defer(rm(list = c("opts", "color"), envir = globalenv()))
  assign("opts", list(parametric = "x"), envir = globalenv())
  assign("color", list(cold = c("#FFF", "#000")), envir = globalenv())

  expect_error(check_opts(color$cold[1]), "color")
})

test_that("check_opts() aborts when the opts object does not exist", {
  if (exists("opts", envir = globalenv(), inherits = FALSE)) {
    rm(list = "opts", envir = globalenv())
  }

  expect_error(check_opts(font), "does not exist")
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
