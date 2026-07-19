test_that("tbl_format() renders a summary (non-coefficient) table to gt_tbl", {
  local_opts()

  res <- tbl_format(gtsum_format(.make_summary_tbl()))

  expect_s3_class(res, "gt_tbl")
})

test_that("tbl_format() attaches note_global as a footnote on a summary table", {
  local_opts()

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    note_global = "global note"
  )

  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "global note",
    all = FALSE
  )
})

test_that("tbl_format() attaches note_vargrp to the targeted variable group", {
  local_opts()

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    note_vargrp = "vargrp note",
    label_vargrp = "age"
  )

  footnotes <- res[["_footnotes"]]

  expect_match(unlist(footnotes$footnotes), "vargrp note", all = FALSE)
  expect_contains(res[["_data"]]$variable[footnotes$rownum], "age")
})

test_that("tbl_format() aborts when note_vargrp is set without label_vargrp", {
  local_opts()

  expect_error(
    tbl_format(gtsum_format(.make_summary_tbl()), note_vargrp = "orphan"),
    "label_vargrp"
  )
})

test_that("tbl_format() appends acronym definitions from acro_list as a footnote", {
  local_opts()

  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    bmi = as.numeric(seq_len(20))
  )
  attr(df$bmi, "label") <- "BMI at baseline"
  tbl <- suppressMessages(gtsummary::tbl_summary(df, by = grp, include = bmi))

  res <- tbl_format(
    gtsum_format(tbl),
    acro_list = list(BMI = "BMI: body mass index")
  )

  expect_match(
    unlist(res[["_footnotes"]]$footnotes),
    "body mass index",
    all = FALSE
  )
})

test_that("tbl_format() sets the table title from `title`", {
  local_opts()

  res <- tbl_format(gtsum_format(.make_summary_tbl()), title = "My Title")

  expect_equal(as.character(res[["_heading"]]$title), "My Title")
})

test_that("tbl_format() routes to a themed flextable under options(hebstr.docx = TRUE)", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()))

  expect_s3_class(res, "flextable")
})

test_that("tbl_format() leaves the gt branch untouched when hebstr.docx is unset", {
  local_opts()
  withr::local_options(hebstr.docx = NULL)

  res <- tbl_format(gtsum_format(.make_summary_tbl()))

  expect_s3_class(res, "gt_tbl")
})

test_that("tbl_format() attaches note_global as a footer line under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    note_global = "global note"
  )

  expect_match(.ft_txt(res), "global note", all = FALSE)
})

test_that("tbl_format() attaches note_vargrp and its body marker under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    note_vargrp = "vargrp note",
    label_vargrp = "age"
  )

  expect_match(.ft_txt(res), "vargrp note", all = FALSE)
  expect_match(.ft_txt(res, "body"), "^age.", all = FALSE)
})

test_that("tbl_format() appends acronym definitions as a footer line under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    bmi = as.numeric(seq_len(20))
  )
  attr(df$bmi, "label") <- "BMI at baseline"
  tbl <- suppressMessages(gtsummary::tbl_summary(df, by = grp, include = bmi))

  res <- tbl_format(
    gtsum_format(tbl),
    acro_list = list(BMI = "BMI: body mass index")
  )

  expect_match(.ft_txt(res), "body mass index", all = FALSE)
})

test_that("tbl_format() sets the table caption from `title` under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), title = "My Title")

  expect_match(paste(unlist(res$caption), collapse = " "), "My Title")
})

test_that("tbl_format() applies zero_replace under docx, and leaves cells alone when NULL", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  tbl <- .make_summary_tbl()
  with_sub <- tbl_format(gtsum_format(tbl))
  without_sub <- tbl_format(gtsum_format(tbl), zero_replace = NULL)

  expect_false(any(grepl("0 (0%)", .ft_txt(with_sub, "body"), fixed = TRUE)))
  expect_true(any(grepl("0 (0%)", .ft_txt(without_sub, "body"), fixed = TRUE)))
})

test_that("tbl_format() registers a value substitution when zero_replace is set, none when NULL", {
  local_opts()

  tbl <- .make_summary_tbl()
  with_sub <- tbl_format(gtsum_format(tbl))
  without_sub <- tbl_format(gtsum_format(tbl), zero_replace = NULL)

  expect_equal(
    length(with_sub[["_substitutions"]]),
    length(without_sub[["_substitutions"]]) + 1L
  )
})

### COLLAPSE MISSING -----------------------------------------------------------

test_that("tbl_format() folds missing rows into a sized dm column on gt", {
  local_opts()

  res <- tbl_format(gtsum_format(.make_missing_tbl()), missing_size = 9)

  expect_true("dm" %in% names(res[["_data"]]))
  expect_false("missing" %in% res[["_data"]]$row_type)

  dm_styles <- res[["_styles"]][res[["_styles"]]$colname %in% "dm", ]
  expect_gt(nrow(dm_styles), 0)
  expect_true(all(
    vapply(
      dm_styles$styles,
      \(s) as.character(s$cell_text$size),
      character(1)
    ) ==
      "9px"
  ))
})

test_that("tbl_format() sizes the dm column on flextable under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_missing_tbl()), missing_size = 9)

  expect_s3_class(res, "flextable")
  expect_true("dm" %in% names(res$body$dataset))

  j <- which(names(res$body$dataset) == "dm")
  expect_equal(
    unique(as.vector(res$body$styles$text$font.size$data[, j])),
    9 * 0.75
  )
})

test_that("tbl_format(collapse_missing = FALSE) keeps missing rows and adds no dm column", {
  local_opts()

  res <- tbl_format(
    gtsum_format(.make_missing_tbl()),
    collapse_missing = FALSE
  )

  expect_false("dm" %in% names(res[["_data"]]))
  expect_true("missing" %in% res[["_data"]]$row_type)
})

test_that("tbl_format() leaves a missing-free table alone and emits no warning by default", {
  local_opts()

  res <- expect_no_warning(tbl_format(gtsum_format(.make_summary_tbl())))
  expect_false("dm" %in% names(res[["_data"]]))
})

test_that("tbl_format() honours tbl_summary(missing = 'no') when rows carry NA row types", {
  local_opts()

  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = c(seq_len(18), NA, NA),
    sex = factor(c(rep("m", 9), NA, rep(c("m", "f"), 5)))
  )

  tbl <-
    suppressMessages(
      gtsummary::tbl_summary(
        df,
        by = grp,
        include = c(age, sex),
        missing = "no"
      )
    ) |>
    gtsum_format() |>
    add_label(name = "Group", levels = c("age", "sex"))

  expect_true(anyNA(tbl$table_body$row_type))
  expect_false("missing" %in% tbl$table_body$row_type)

  res <- expect_no_warning(tbl_format(tbl))

  expect_false("dm" %in% names(res[["_data"]]))
})

test_that("tbl_format() aborts when collapse_missing is not a logical scalar", {
  local_opts()

  expect_error(
    tbl_format(gtsum_format(.make_missing_tbl()), collapse_missing = "ifany"),
    "collapse_missing"
  )
})

### DEPRECATED ALIAS -----------------------------------------------------------

test_that("gt_format() warns once and delegates to tbl_format()", {
  local_opts()

  tbl <- gtsum_format(.make_summary_tbl())

  expect_snapshot(res <- gt_format(tbl))
  expect_s3_class(res, "gt_tbl")
})

test_that("gt_format() forwards its arguments to tbl_format()", {
  local_opts()

  tbl <- gtsum_format(.make_summary_tbl())

  res <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    gt_format(tbl, title = "My Title")
  )

  expect_equal(as.character(res[["_heading"]]$title), "My Title")
})
