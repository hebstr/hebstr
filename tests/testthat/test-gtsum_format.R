.make_reg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15)
  )
  mod <- glm(y ~ x, data = df, family = binomial())

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
}

.make_uvreg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15),
    z = seq_len(30) - rep(c(0, 1), 15)
  )
  gtsummary::tbl_uvregression(
    df,
    method = glm,
    y = y,
    method.args = list(family = binomial()),
    exponentiate = TRUE
  )
}

.make_no_event_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    g = factor(rep(c("a", "b", "c"), 10))
  )
  df$y[df$g == "c"] <- 0

  suppressWarnings(
    gtsummary::tbl_uvregression(
      df,
      method = glm,
      y = y,
      method.args = list(family = binomial()),
      exponentiate = TRUE
    )
  )
}

.make_summary_df <- \() {
  data.frame(
    grp = rep(c("a", "b"), 10),
    age = c(1:10, 5:14),
    sex = factor(rep(c("m", "f"), 10))
  )
}

test_that("gtsum_format() routes a by-group summary through the by-table formatter", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  tbl <- suppressMessages(
    gtsummary::tbl_summary(
      .make_summary_df(),
      by = grp,
      include = c(age, sex)
    ) |>
      gtsummary::add_p()
  )

  res <- expect_no_warning(gtsum_format(tbl))

  expect_s3_class(res, "gtsummary")
  expect_contains(names(res$table_body), c("stat_0", "stat_1", "stat_2"))
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "p.value"], "**p**")
})

test_that("gtsum_format() routes a univariate summary through the uni formatter", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  tbl <- suppressMessages(
    gtsummary::tbl_summary(
      .make_summary_df(),
      include = c(age, sex)
    )
  )

  res <- expect_no_warning(gtsum_format(tbl, label_stat = "Total"))

  expect_s3_class(res, "gtsummary")
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "label"], "**Characteristic**")
  expect_equal(header$label[header$column == "stat_0"], "**Total**")
})

test_that("gtsum_format() returns its result visibly", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()

  expect_true(withVisible(gtsum_format(reg$tbl, model_mv = reg$mod))$visible)
})

test_that("gtsum_format() records the estim annotation internally, not in globalenv", {
  withr::defer(rm(list = "opts", envir = .hebstr))
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

test_that("tbl_format() aborts informatively on a coefficient table without annotation", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()
  formatted <- gtsum_format(reg$tbl, model_mv = reg$mod)

  rm(list = ls(envir = .estim_channel), envir = .estim_channel)

  expect_error(tbl_format(formatted), "gtsum_format")
})

test_that("gtsum_format() does not require model_mv when show_single_row is FALSE", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()

  expect_s3_class(gtsum_format(reg$tbl), "gtsummary")
})

test_that("gtsum_format() aborts when show_single_row is TRUE without model_mv", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()

  expect_error(
    gtsum_format(reg$tbl, show_single_row = TRUE),
    "show_single_row"
  )
})

test_that("gtsum_format() annotates dichotomous reference levels with show_single_row", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  df <- data.frame(
    y = rep(c(0, 1, 1, 0, 1, 0), 10),
    sex = factor(rep(c("f", "m"), 30), levels = c("f", "m")),
    x = seq_len(60)
  )
  mod <- glm(y ~ sex + x, data = df, family = binomial())
  tbl <- suppressWarnings(suppressMessages(
    gtsummary::tbl_regression(mod, exponentiate = TRUE, show_single_row = sex)
  ))

  res <- gtsum_format(tbl, model_mv = mod, show_single_row = TRUE)

  sex_label <- as.character(res$table_body$label[
    res$table_body$variable == "sex"
  ])
  expect_match(sex_label, "ref")
  expect_match(sex_label, "m")
  expect_match(sex_label, "f")
  cont_label <- as.character(res$table_body$label[
    res$table_body$variable == "x"
  ])
  expect_equal(unname(cont_label), "x")
})

test_that("gtsum_format() forces vargrp_levels rows to indented levels", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  tbl <- suppressMessages(
    gtsummary::tbl_summary(.make_summary_df(), include = c(age, sex))
  )

  res <- gtsum_format(tbl, vargrp_levels = "age", indent = 12)

  age_row_type <- unique(
    res$table_body$row_type[res$table_body$variable == "age"]
  )
  expect_equal(age_row_type, "level")
  expect_contains(res$table_styling$indent$n_spaces, 12L)
})

test_that("gtsum_format() routes a univariate regression through the n-column formatter", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  uv <- suppressMessages(.make_uvreg_tbl())
  res <- expect_no_warning(gtsum_format(uv))

  expect_s3_class(res, "tbl_uvregression")
  expect_contains(names(res$table_body), "stat_n")
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "stat_n"], "**n/N**")
})

test_that("gtsum_format() routes a multivariable regression through the n-column formatter", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()
  res <- expect_no_warning(gtsum_format(reg$tbl, model_mv = reg$mod))

  expect_s3_class(res, "tbl_regression")
  expect_contains(names(res$table_body), "stat_n")
  header <- res$table_styling$header
  expect_equal(header$label[header$column == "stat_n"], "**n/N**")
})

test_that("gtsum_format() blanks the estimate of a level carrying no event", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- suppressMessages(gtsum_format(.make_no_event_tbl()))

  body <- res$table_body
  no_event <- body$n_event %in% 0

  expect_true(any(no_event))
  expect_true(all(is.na(body$estimate[no_event])))
  expect_true(all(is.na(body$ci[no_event])))
})

test_that("gtsum_format() keeps the estimate of a level carrying events", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- suppressMessages(gtsum_format(.make_no_event_tbl()))

  body <- res$table_body
  estimable <- body$n_event > 0 & body$row_type == "level" & !body$reference_row

  expect_true(any(estimable, na.rm = TRUE))
  expect_false(anyNA(body$estimate[which(estimable)]))
})

test_that("gtsum_format() leaves an eventless regression table untouched", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  df <- data.frame(y = seq_len(30) + rep(c(0, 2), 15), x = seq_len(30))
  lm_tbl <- gtsummary::tbl_uvregression(df, method = lm, y = y)

  res <- expect_no_warning(gtsum_format(lm_tbl))

  expect_false("n_event" %in% names(res$table_body))
  expect_false(anyNA(res$table_body$estimate[res$table_body$variable == "x"]))
})

test_that("tbl_format() aborts informatively on a non-gtsummary input", {
  expect_error(tbl_format(mtcars), "gtsummary")
})

test_that("tbl_format() attaches note_pvalue to the p-value column", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()
  res <- tbl_format(
    gtsum_format(reg$tbl, model_mv = reg$mod),
    note_pvalue = "Wald test"
  )

  expect_s3_class(res, "gt_tbl")
  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "Wald test",
    all = FALSE
  )
})

test_that("gtsum_format() |> tbl_format() renders the estimator footnote", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()
  res <- tbl_format(gtsum_format(reg$tbl, model_mv = reg$mod))

  expect_s3_class(res, "gt_tbl")
  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "odds ratio",
    all = FALSE
  )
})

test_that("tbl_format() strips the native gtsummary abbreviations", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  reg <- .make_reg_tbl()

  expect_match(
    reg$tbl$table_styling$abbreviation$abbreviation,
    "Confidence Interval",
    all = FALSE
  )

  res <- tbl_format(gtsum_format(reg$tbl, model_mv = reg$mod))

  expect_false(any(str_detect(
    unlist(res[["_footnotes"]]$footnotes),
    "Confidence Interval"
  )))
})
