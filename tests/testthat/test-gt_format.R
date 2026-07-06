.make_summary_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = seq_len(20),
    sex = factor(c(rep("m", 10), rep(c("m", "f"), 5)))
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = c(age, sex))
  )
}

test_that("gt_format() renders a summary (non-coefficient) table to gt_tbl", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- gt_format(gtsum_format(.make_summary_tbl()))

  expect_s3_class(res, "gt_tbl")
})

test_that("gt_format() attaches note_global as a footnote on a summary table", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- gt_format(
    gtsum_format(.make_summary_tbl()),
    note_global = "global note"
  )

  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "global note",
    all = FALSE
  )
})

test_that("gt_format() attaches note_vargrp to the targeted variable group", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- gt_format(
    gtsum_format(.make_summary_tbl()),
    note_vargrp = "vargrp note",
    label_vargrp = "age"
  )

  footnotes <- res[["_footnotes"]]

  expect_match(unlist(footnotes$footnotes), "vargrp note", all = FALSE)
  expect_contains(res[["_data"]]$variable[footnotes$rownum], "age")
})

test_that("gt_format() aborts when note_vargrp is set without label_vargrp", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  expect_error(
    gt_format(gtsum_format(.make_summary_tbl()), note_vargrp = "orphan"),
    "label_vargrp"
  )
})

test_that("gt_format() appends acronym definitions from acro_list as a footnote", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    bmi = as.numeric(seq_len(20))
  )
  attr(df$bmi, "label") <- "BMI at baseline"
  tbl <- suppressMessages(gtsummary::tbl_summary(df, by = grp, include = bmi))

  res <- gt_format(
    gtsum_format(tbl),
    acro_list = list(BMI = "BMI: body mass index")
  )

  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "body mass index",
    all = FALSE
  )
})

test_that("gt_format() sets the table title from `title`", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  res <- gt_format(gtsum_format(.make_summary_tbl()), title = "My Title")

  expect_equal(as.character(res[["_heading"]]$title), "My Title")
})

test_that("gt_format() registers a value substitution when zero_replace is set, none when NULL", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  tbl <- .make_summary_tbl()
  with_sub <- gt_format(gtsum_format(tbl))
  without_sub <- gt_format(gtsum_format(tbl), zero_replace = NULL)

  expect_equal(
    length(with_sub[["_substitutions"]]),
    length(without_sub[["_substitutions"]]) + 1L
  )
})
