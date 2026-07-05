test_that("gt_heatmap applies color styling to a numeric table", {
  df <- data.frame(cat = c("a", "b", "c"), x = c(1, 2, 3), y = c(2, 4, 6))

  res <- gt_heatmap(df, rowname_col = "cat")

  expect_s3_class(res, "gt_tbl")
  expect_gt(nrow(res$`_styles`), 0)
})

test_that("gt_heatmap does not error when data contains NA values", {
  df <- data.frame(cat = c("a", "b", "c"), x = c(1, NA, 3), y = c(2, 4, NA))

  res <- gt_heatmap(df, rowname_col = "cat")

  expect_s3_class(res, "gt_tbl")
  expect_gt(nrow(res$`_styles`), 0)
})

test_that("gt_heatmap builds a cross-tab from two columns with identical levels", {
  df <- data.frame(
    rater1 = factor(c("a", "b", "a", "b", "a")),
    rater2 = factor(c("a", "a", "b", "b", "a"))
  )

  res <- gt_heatmap(df)

  expect_s3_class(res, "gt_tbl")
  expect_named(res$`_data`, c("rater1", "a", "b"))
  expect_equal(nrow(res$`_data`), 2)
})

test_that("gt_heatmap still returns a table when color is disabled", {
  df <- data.frame(cat = c("a", "b", "c"), x = c(1, 2, 3))

  res <- gt_heatmap(df, rowname_col = "cat", color = FALSE)

  expect_s3_class(res, "gt_tbl")
  expect_gt(nrow(res$`_styles`), 0)
})

test_that("gt_heatmap default font_family reads the centralised text font", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = globalenv())

  df <- data.frame(cat = c("a", "b", "c"), x = c(1, 2, 3), y = c(2, 4, 6))

  res <- gt_heatmap(df, rowname_col = "cat")

  table_font <- res$`_options`$value[[
    which(res$`_options`$parameter == "table_font_names")
  ]]

  expect_identical(table_font[[1]], "PinnedAlpha")
})
