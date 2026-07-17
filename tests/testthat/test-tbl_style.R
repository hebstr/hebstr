.tbl_data <- \() {
  data.frame(
    label = c("Alpha", "Beta", "Gamma"),
    stat_1 = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
}

.make_gt <- \() gt::gt(.tbl_data())

.make_ft <- \() flextable::flextable(.tbl_data())

### GT FONT SIZE ---------------------------------------------------------------

test_that("tbl_font_size() styles the targeted column of a gt table", {
  res <- tbl_font_size(.make_gt(), columns = stat_1, size = 11)

  expect_s3_class(res, "gt_tbl")
  expect_equal(nrow(res$`_styles`), nrow(.tbl_data()))
  expect_setequal(res$`_styles`$colname, "stat_1")
  expect_equal(res$`_styles`$styles[[1]]$cell_text$size, gt::px(11))
})

test_that("tbl_font_size() converts pixels to points on a flextable", {
  res <- tbl_font_size(.make_ft(), columns = stat_1, size = 11)

  expect_s3_class(res, "flextable")
  expect_true(all(res$body$styles$text$font.size$data[, "stat_1"] == 8.25))
})

test_that("tbl_font_size() leaves untargeted columns alone on a flextable", {
  ref <- .make_ft()$body$styles$text$font.size$data[, "label"]

  res <- tbl_font_size(.make_ft(), columns = stat_1, size = 11)

  expect_equal(res$body$styles$text$font.size$data[, "label"], ref)
})

test_that("tbl_font_size() accepts tidyselect helpers", {
  res <- tbl_font_size(.make_ft(), columns = starts_with("stat"), size = 11)

  expect_true(all(res$body$styles$text$font.size$data[, "stat_1"] == 8.25))
})

test_that("tbl_font_size() rejects an unsupported object", {
  expect_error(
    tbl_font_size(.tbl_data(), columns = stat_1, size = 11),
    "gt_tbl"
  )
})

test_that("tbl_font_size() rejects a non-numeric size", {
  expect_error(tbl_font_size(.make_gt(), columns = stat_1, size = "11"), "size")
})

### GT ROW COLOR ---------------------------------------------------------------

test_that("tbl_row_color() styles the matching rows of a gt table", {
  res <- tbl_row_color(.make_gt(), rows = label == "Beta", color = "#FF0000")

  expect_s3_class(res, "gt_tbl")
  expect_setequal(res$`_styles`$rownum, 2)
  expect_equal(res$`_styles`$styles[[1]]$cell_text$color, "#FF0000")
})

test_that("tbl_row_color() colors the matching rows of a flextable", {
  res <- tbl_row_color(.make_ft(), rows = label == "Beta", color = "#FF0000")

  expect_s3_class(res, "flextable")
  expect_equal(
    unname(res$body$styles$text$color$data[, "label"]),
    c("black", "#FF0000", "black")
  )
})

test_that("tbl_row_color() colors every column of the matching flextable row", {
  res <- tbl_row_color(.make_ft(), rows = label == "Beta", color = "#FF0000")

  expect_equal(unname(res$body$styles$text$color$data[2, ]), rep("#FF0000", 2))
})

test_that("tbl_row_color() matching no row leaves the flextable unstyled", {
  res <- tbl_row_color(.make_ft(), rows = label == "Delta", color = "#FF0000")

  expect_false(any(res$body$styles$text$color$data == "#FF0000"))
})

test_that("tbl_row_color() rejects an unsupported object", {
  expect_error(
    tbl_row_color(.tbl_data(), rows = label == "Beta", color = "#FF0000"),
    "gt_tbl"
  )
})

### INTEGRATION WITH tbl_format() -----------------------------------------------

.styles_for <- \(x, col) {
  .keep <- !is.na(x$`_styles`$colname) & x$`_styles`$colname == col

  x$`_styles`$styles[.keep]
}

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

test_that("tbl_font_size() styles a tbl_format() table on both branches", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  withr::with_options(list(hebstr.docx = NULL), {
    res_gt <- tbl_format(gtsum_format(.make_summary_tbl())) |>
      tbl_font_size(columns = stat_1, size = 11)
  })

  withr::with_options(list(hebstr.docx = TRUE), {
    res_ft <- tbl_format(gtsum_format(.make_summary_tbl())) |>
      tbl_font_size(columns = stat_1, size = 11)
  })

  expect_s3_class(res_gt, "gt_tbl")
  expect_true(any(vapply(
    .styles_for(res_gt, "stat_1"),
    \(s) identical(s$cell_text$size, gt::px(11)),
    logical(1)
  )))

  expect_s3_class(res_ft, "flextable")
  expect_true(all(res_ft$body$styles$text$font.size$data[, "stat_1"] == 8.25))
})

test_that("tbl_row_color() styles a tbl_format() table on both branches", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  set_opts()

  withr::with_options(list(hebstr.docx = NULL), {
    res_gt <- tbl_format(gtsum_format(.make_summary_tbl())) |>
      tbl_row_color(rows = label == "age", color = "#FF0000")
  })

  withr::with_options(list(hebstr.docx = TRUE), {
    res_ft <- tbl_format(gtsum_format(.make_summary_tbl())) |>
      tbl_row_color(rows = label == "age", color = "#FF0000")
  })

  expect_s3_class(res_gt, "gt_tbl")
  expect_true(any(vapply(
    res_gt$`_styles`$styles,
    \(s) identical(s$cell_text$color, "#FF0000"),
    logical(1)
  )))

  expect_s3_class(res_ft, "flextable")
  expect_true(any(res_ft$body$styles$text$color$data == "#FF0000"))
})
