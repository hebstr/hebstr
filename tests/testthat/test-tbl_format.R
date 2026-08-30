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

test_that("tbl_format(title_align) reaches the caption under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(.make_summary_tbl(), title = "T", title_align = "center")

  expect_identical(res$caption$fp_p$text.align, "center")
})

test_that("tbl_format() gives both branches the same title alignment by default", {
  local_opts()

  gt_tbl <- withr::with_options(
    list(hebstr.docx = NULL),
    tbl_format(.make_summary_tbl(), title = "T")
  )
  ft <- withr::with_options(
    list(hebstr.docx = TRUE),
    tbl_format(.make_summary_tbl(), title = "T")
  )

  expect_identical(.gt_align(gt_tbl, "title"), "justify")
  expect_identical(ft$caption$fp_p$text.align, "justify")
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

test_that("tbl_format() applies zero_replace on gt, and leaves cells alone when NULL", {
  local_opts()

  tbl <- .make_summary_tbl()
  with_sub <- gt::as_raw_html(tbl_format(gtsum_format(tbl)))
  without_sub <- gt::as_raw_html(tbl_format(
    gtsum_format(tbl),
    zero_replace = NULL
  ))

  expect_no_match(with_sub, "0 (0%)", fixed = TRUE)
  expect_match(without_sub, "0 (0%)", fixed = TRUE)
  expect_match(with_sub, "10 (100%)", fixed = TRUE)
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

test_that("gt_format() warns of its deprecation and delegates to tbl_format()", {
  local_opts()
  withr::local_options(lifecycle_verbosity = "warning")

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

test_that("tbl_format(width = ) sizes the gt table in pixels", {
  local_opts()
  withr::local_options(hebstr.docx = NULL)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), width = 500)

  expect_equal(
    res[["_options"]]$value[[which(
      res[["_options"]]$parameter == "table_width"
    )]],
    "500px"
  )
})

test_that("tbl_format(width = ) converts pixels to a page fraction under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), width = 500)

  expect_s3_class(res, "flextable")
  expect_equal(res$properties$width, 500 / (6.5 * 96))
})

test_that("tbl_format(width = ) caps the docx page fraction at 1", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), width = 5000)

  expect_equal(res$properties$width, 1)
})

test_that("tbl_format(page_width = ) sets the reference page width in inches", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    width = 500,
    page_width = 8
  )

  expect_equal(res$properties$width, 500 / (8 * 96))
})

test_that("tbl_format(width = ) rejects a non-positive or non-scalar width", {
  local_opts()

  tbl <- gtsum_format(.make_summary_tbl())

  expect_error(tbl_format(tbl, width = -1), "must be a single positive number")
  expect_error(
    tbl_format(tbl, width = c(1, 2)),
    "must be a single positive number"
  )
  expect_error(
    tbl_format(tbl, width = 500, page_width = 0),
    "must be a single positive number"
  )
})

test_that("tbl_format() renders a coefficient table to a flextable under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(suppressMessages(.make_uvreg_tbl())))

  expect_s3_class(res, "flextable")
  expect_contains(res$body$dataset$stat_n, "15/30")
})

test_that("tbl_format() renders the <br> of gtsummary headers as a line break under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()))
  labels <- .ft_txt(res, part = "header")

  expect_false(any(stringr::str_detect(labels, stringr::fixed("<br>"))))
  expect_true(any(stringr::str_detect(labels, stringr::fixed("\n"))))
  expect_false(any(stringr::str_detect(labels, stringr::fixed("**"))))
})

test_that("tbl_format() keeps the <br> of gtsummary headers on the gt branch", {
  local_opts()
  withr::local_options(hebstr.docx = NULL)

  res <- tbl_format(gtsum_format(.make_summary_tbl()))
  labels <- res[["_boxhead"]]$column_label |> unlist()

  expect_true(any(stringr::str_detect(labels, stringr::fixed("<br>"))))
})

test_that("tbl_format() renders a docx footnote symbol without the flextable big.mark warning", {
  withr::local_options(hebstr.docx = TRUE, OutDec = ",")
  local_opts()

  expect_no_warning(
    tbl_format(
      gtsum_format(.make_summary_tbl()),
      label_vargrp = "age",
      note_vargrp = "vargrp note"
    )
  )
})

test_that("tbl_format() leaves the adjusted estimator out of a univariate footnote", {
  local_opts()

  res <- tbl_format(gtsum_format(suppressMessages(.make_uvreg_tbl())))
  notes <- unlist(res[["_footnotes"]]$footnotes)

  expect_match(notes, "odds ratio", all = FALSE)
  expect_false(any(stringr::str_detect(notes, "adjusted")))
})

test_that("tbl_format() keeps the gtsummary row indentation under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_format(gtsum_format(.make_summary_tbl()))
  lead <- res$body$styles$pars$padding.left$data[, 1]
  rows <- res$body$dataset$label

  expect_gt(lead[rows == "m"], lead[rows == "sex"])
})

test_that("tbl_format() runs note_global and the acronym note into one footnote", {
  local_opts()

  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    bmi = as.numeric(seq_len(20))
  )
  attr(df$bmi, "label") <- "BMI at baseline"
  tbl <- suppressMessages(gtsummary::tbl_summary(df, by = grp, include = bmi))

  res <- tbl_format(
    gtsum_format(tbl),
    note_global = "global note",
    acro_list = list(BMI = "BMI: body mass index")
  )

  html <- gt::as_raw_html(res)

  expect_length(
    unlist(stringr::str_extract_all(html, 'class="gt_footnote"')),
    1
  )
  expect_match(html, "global note BMI: body mass index.", fixed = TRUE)
})

test_that("tbl_format() reads page_width from the options", {
  withr::local_options(hebstr.docx = TRUE)
  local_opts(page_width = 8)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), width = 500)

  expect_equal(res$properties$width, 500 / (8 * 96))
})

test_that("tbl_format(page_width = ) overrides the option", {
  withr::local_options(hebstr.docx = TRUE)
  local_opts(page_width = 8)

  res <- tbl_format(
    gtsum_format(.make_summary_tbl()),
    width = 500,
    page_width = 6.5
  )

  expect_equal(res$properties$width, 500 / (6.5 * 96))
})

test_that("tbl_format() resolves the template when no page_width is set", {
  withr::local_options(hebstr.docx = TRUE)
  local_opts()

  local_mocked_bindings(.page_width = \(...) 8)

  res <- tbl_format(gtsum_format(.make_summary_tbl()), width = 500)

  expect_equal(res$properties$width, 500 / (8 * 96))
})

test_that("tbl_format() leaves the template unresolved on the gt branch", {
  local_opts()

  local_mocked_bindings(
    .page_width = \(...) cli::cli_abort("resolved on the gt branch")
  )

  expect_no_error(tbl_format(gtsum_format(.make_summary_tbl()), width = 500))
})
