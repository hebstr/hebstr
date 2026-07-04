.make_reg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15)
  )
  mod <- glm(y ~ x, data = df, family = binomial())

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
}

.make_summary_df <- \() {
  data.frame(
    grp = rep(c("a", "b"), 10),
    age = c(1:10, 5:14),
    sex = factor(rep(c("m", "f"), 10))
  )
}

test_that("gtsum_format() routes a by-group summary through the by-table formatter", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  tbl <- suppressMessages(
    gtsummary::tbl_summary(
      .make_summary_df(),
      by = grp,
      include = c(age, sex)
    ) |>
      gtsummary::add_p()
  )

  res <- gtsum_format(tbl)

  expect_s3_class(res, "gtsummary")
  expect_true(all(c("stat_0", "stat_1", "stat_2") %in% names(res$table_body)))
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "p.value"], "**p**")
})

test_that("gtsum_format() routes a univariate summary through the uni formatter", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  tbl <- suppressMessages(
    gtsummary::tbl_summary(
      .make_summary_df(),
      include = c(age, sex)
    )
  )

  res <- gtsum_format(tbl, label_stat = "Total")

  expect_s3_class(res, "gtsummary")
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "label"], "**Characteristic**")
  expect_equal(header$label[header$column == "stat_0"], "**Total**")
})

test_that("gtsum_format() returns its result visibly", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()

  expect_true(withVisible(gtsum_format(reg$tbl, model_mv = reg$mod))$visible)
})

test_that("gtsum_format() records the estim annotation internally, not in globalenv", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  if (exists(".estim", envir = globalenv(), inherits = FALSE)) {
    rm(list = ".estim", envir = globalenv())
  }

  reg <- .make_reg_tbl()
  gtsum_format(reg$tbl, model_mv = reg$mod)

  expect_false(exists(".estim", envir = globalenv(), inherits = FALSE))
  expect_true(exists("estim", envir = .estim_channel, inherits = FALSE))
  expect_named(.estim_channel$estim, c("uv", "mv"))
})

test_that("gt_format() aborts informatively on a coefficient table without annotation", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()
  formatted <- gtsum_format(reg$tbl, model_mv = reg$mod)

  rm(list = ls(envir = .estim_channel), envir = .estim_channel)

  expect_error(gt_format(formatted), "gtsum_format")
})

test_that("gtsum_format() does not require model_mv when show_single_row is FALSE", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()

  expect_s3_class(gtsum_format(reg$tbl), "gtsummary")
})

test_that("gtsum_format() aborts when show_single_row is TRUE without model_mv", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()

  expect_error(
    gtsum_format(reg$tbl, show_single_row = TRUE),
    "show_single_row"
  )
})

test_that("gt_format() aborts informatively on a non-gtsummary input", {
  expect_error(gt_format(mtcars), "gtsummary")
})

test_that("gt_format() attaches note_pvalue to the p-value column", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()
  res <- gt_format(
    gtsum_format(reg$tbl, model_mv = reg$mod),
    note_pvalue = "Wald test"
  )

  expect_s3_class(res, "gt_tbl")
  expect_true(any(str_detect(
    unlist(res[["_footnotes"]]$footnotes),
    "Wald test"
  )))
})

test_that("gtsum_format() |> gt_format() renders the estimator footnote", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  set_opts()

  reg <- .make_reg_tbl()
  res <- gt_format(gtsum_format(reg$tbl, model_mv = reg$mod))

  expect_s3_class(res, "gt_tbl")
  expect_true(any(str_detect(
    unlist(res[["_footnotes"]]$footnotes),
    "odds ratio"
  )))
})
