test_that("merge_estim_ci() merges estimate and CI into a single column", {
  df <- data.frame(
    estimate = c(1.234, 2.567),
    conf.low = c(0.5, 1.2),
    conf.high = c(2.0, 3.9)
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, ci_data = ci_template)

  expect_true("estimate_ci" %in% names(result))
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

  expect_true("estimate_ci" %in% names(result))
  expect_true("estimate" %in% names(result))
  expect_true("conf.low" %in% names(result))
  expect_true("conf.high" %in% names(result))
})

test_that("merge_estim_ci() uses custom output column name", {
  df <- data.frame(
    estimate = 1.5,
    conf.low = 1.0,
    conf.high = 2.0
  )

  ci_template <- "[{conf.low}; {conf.high}]"

  result <- merge_estim_ci(df, name = "OR_CI", ci_data = ci_template)

  expect_true("OR_CI" %in% names(result))
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

  expect_true("estimate_ci" %in% names(result))
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

  expect_true("term" %in% names(result))
  expect_equal(result$term, c("age", "sex"))
})
