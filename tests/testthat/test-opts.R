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
