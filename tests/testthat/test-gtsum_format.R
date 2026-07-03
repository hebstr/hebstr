.make_reg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15)
  )
  mod <- glm(y ~ x, data = df, family = binomial())

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
}

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
