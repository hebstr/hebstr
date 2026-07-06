test_that("merge_estim_ci() merges estimate and CI into a single column", {
  df <- data.frame(
    estimate = c(1.234, 2.567),
    conf.low = c(0.5, 1.2),
    conf.high = c(2.0, 3.9)
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template)

  expect_contains(names(result), "estimate_ci")
  expect_false("estimate" %in% names(result))
  expect_false("conf.low" %in% names(result))
  expect_false("conf.high" %in% names(result))
})

test_that("merge_estim_ci() formats values with 2 decimal places", {
  df <- data.frame(
    estimate = 1.1,
    conf.low = 0.5,
    conf.high = 2.0
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template)

  expect_match(result$estimate_ci, "1.10")
  expect_match(result$estimate_ci, "0.50")
  expect_match(result$estimate_ci, "2.00")
})

test_that("merge_estim_ci() multiplies by 100 when percent = TRUE", {
  df <- data.frame(
    estimate = 0.25,
    conf.low = 0.10,
    conf.high = 0.40
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template, percent = TRUE)

  expect_match(result$estimate_ci, "25.00")
  expect_match(result$estimate_ci, "10.00")
  expect_match(result$estimate_ci, "40.00")
})

test_that("merge_estim_ci() keeps original columns when keep = TRUE", {
  df <- data.frame(
    estimate = 1.5,
    conf.low = 1.0,
    conf.high = 2.0
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template, keep = TRUE)

  expect_contains(names(result), "estimate_ci")
  expect_contains(names(result), "estimate")
  expect_contains(names(result), "conf.low")
  expect_contains(names(result), "conf.high")

  expect_type(result$estimate, "double")
  expect_equal(result$estimate, 1.5)
  expect_equal(result$conf.low, 1.0)
  expect_equal(result$conf.high, 2.0)
})

test_that("merge_estim_ci() uses custom output column name", {
  df <- data.frame(
    estimate = 1.5,
    conf.low = 1.0,
    conf.high = 2.0
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, name = "OR_CI", ci_data = ci_template)

  expect_contains(names(result), "OR_CI")
  expect_false("estimate_ci" %in% names(result))
})

test_that("merge_estim_ci() works with custom estimate column name", {
  df <- data.frame(
    OR = 2.5,
    conf.low = 1.2,
    conf.high = 5.1
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(
    data = df,
    estim_col = "OR",
    ci_data = ci_template
  )

  expect_contains(names(result), "estimate_ci")
  expect_match(result$estimate_ci, "2.50")
})

test_that("merge_estim_ci() preserves other columns in the data", {
  df <- data.frame(
    term = c("age", "sex"),
    estimate = c(1.2, 0.8),
    conf.low = c(0.9, 0.5),
    conf.high = c(1.5, 1.1)
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template)

  expect_contains(names(result), "term")
  expect_equal(result$term, c("age", "sex"))
})

test_that("merge_estim_ci() does not pad values of differing widths", {
  df <- data.frame(
    estimate = c(1.2, 10.5),
    conf.low = c(0.5, 9.1),
    conf.high = c(2.0, 12.0)
  )

  result <- merge_estim_ci(df, ci_data = "[{conf.low}; {conf.high}]")

  expect_equal(
    result$estimate_ci,
    c("1.20 [0.50; 2.00]", "10.50 [9.10; 12.00]")
  )
})

test_that("merge_estim_ci() accepts a custom ci_col tidy-selection", {
  df <- data.frame(
    estimate = 1.5,
    ci_low = 1.0,
    ci_high = 2.0
  )

  result <- merge_estim_ci(
    df,
    ci_col = starts_with("ci_"),
    ci_data = "[{ci_low}; {ci_high}]"
  )

  expect_equal(result$estimate_ci, "1.50 [1.00; 2.00]")
  expect_false("ci_low" %in% names(result))
  expect_false("ci_high" %in% names(result))
})

test_that("merge_estim_ci() interpolates glue in the output column name", {
  df <- data.frame(
    OR = 2.5,
    conf.low = 1.2,
    conf.high = 5.1
  )

  result <- merge_estim_ci(
    df,
    estim_col = "OR",
    name = "{estim_col}_ci",
    ci_data = "[{conf.low}; {conf.high}]"
  )

  expect_contains(names(result), "OR_ci")
  expect_false("estimate_ci" %in% names(result))
})

test_that("merge_estim_ci() derives the CI template from options when ci_data is omitted", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  withr::local_options(OutDec = ".")
  set_opts()

  df <- data.frame(
    estimate = 1.2,
    conf.low = 0.5,
    conf.high = 2.0
  )

  result <- merge_estim_ci(df)

  expect_equal(result$estimate_ci, "1.20 [0.50; 2.00]")
})

test_that("merge_estim_ci() formats decimals with the FR locale (OutDec = ',')", {
  withr::local_options(OutDec = ",")

  df <- data.frame(
    estimate = 1.2,
    conf.low = 0.5,
    conf.high = 2.0
  )

  result <- merge_estim_ci(df, ci_data = "[{conf.low}; {conf.high}]")

  expect_equal(result$estimate_ci, "1,20 [0,50; 2,00]")
})
