test_that("quanti.test.para returns a tidy ANOVA result with more than 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7, 15, 14, 16),
    g = rep(c("a", "b", "c"), each = 3)
  )

  res <- quanti.test.para(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_contains(names(res), "p.value")
  expect_equal(nrow(res), 1)
  expect_equal(
    res$p.value,
    oneway.test(y ~ g, data = data, var.equal = TRUE)$p.value
  )
})

test_that("quanti.test.nonpara runs a Wilcoxon test with 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7),
    g = rep(c("a", "b"), each = 3)
  )

  res <- quanti.test.nonpara(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_match(res$method, "Wilcoxon")
  expect_equal(
    res$p.value,
    suppressWarnings(
      wilcox.test(y ~ g, data = data, exact = FALSE, correct = FALSE)$p.value
    )
  )
})

test_that("quanti.test.nonpara runs a Kruskal-Wallis test with more than 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7, 15, 14, 16),
    g = rep(c("a", "b", "c"), each = 3)
  )

  res <- quanti.test.nonpara(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_match(res$method, "Kruskal-Wallis")
  expect_equal(
    res$p.value,
    kruskal.test(y ~ g, data = data)$p.value
  )
})

test_that("quali.test uses an uncorrected chi-squared when all expected counts are >= 5", {
  data <- data.frame(
    v = c(rep("A", 30), rep("B", 30)),
    g = c(rep("a", 15), rep("b", 15), rep("a", 15), rep("b", 15))
  )

  res <- quali.test(data, "v", "g")

  expect_s3_class(res, "data.frame")
  expect_equal(res$method, "Pearson's Chi-squared test")
  expect_equal(
    res$p.value,
    suppressWarnings(chisq.test(data$v, data$g, correct = FALSE)$p.value)
  )
})

test_that("quali.test applies Yates' correction when an expected count is between 2 and 5", {
  data <- data.frame(
    v = c(rep("A", 8), rep("B", 7)),
    g = c(rep("a", 4), rep("b", 4), rep("a", 1), rep("b", 6))
  )

  res <- quali.test(data, "v", "g")

  expect_s3_class(res, "data.frame")
  expect_match(res$method, "Yates")
  expect_equal(
    res$p.value,
    suppressWarnings(chisq.test(data$v, data$g, correct = TRUE)$p.value)
  )
})

test_that("quali.test falls back to Fisher's exact test when an expected count is below 2", {
  data <- data.frame(
    v = c(rep("A", 3), rep("B", 3)),
    g = c(rep("a", 3), rep("b", 3))
  )

  res <- quali.test(data, "v", "g")

  expect_s3_class(res, "data.frame")
  expect_match(res$method, "Fisher")
  expect_equal(
    res$p.value,
    fisher.test(data$v, factor(data$g))$p.value
  )
})

test_that("fct_other_str combines rare factor levels and character counts", {
  fct <- factor(c("Alpha", "Alpha", "Alpha", "Beta", "Gamma", "Other", "Other"))
  chr <- c("x y", "x y", "z")

  res <- fct_other_str(fct, chr, min = 2)

  expect_equal(as.character(res), "Beta (1), gamma (1), x y [2], z [1].")
})

test_that("fct_other_str forwards sep to the character counts", {
  fct <- factor(c("Alpha", "Alpha", "Alpha", "Beta", "Gamma", "Other", "Other"))
  chr <- c("x y", "x y", "z")

  res <- fct_other_str(fct, chr, min = 2, sep = " ; ")

  expect_equal(as.character(res), "Beta (1), gamma (1), x y [2] ; z [1].")
})

test_that("fct_other_str drops the empty rare-level part instead of leaving a leading separator", {
  fct <- factor(c("Alpha", "Alpha", "Beta", "Beta"))
  chr <- c("x y", "z")

  res <- fct_other_str(fct, chr, min = 2)

  expect_equal(as.character(res), "x y [1], z [1].")
})

test_that("quanti.test.para still runs a t-test with 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7),
    g = rep(c("a", "b"), each = 3)
  )

  res <- quanti.test.para(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_equal(
    res$p.value,
    t.test(y ~ g, data = data, var.equal = TRUE)$p.value
  )
})

test_that("fct_keep splits levels above/below min into keep/drop", {
  data <- data.frame(g = c("a", "a", "a", "b", "c"))

  res <- fct_keep(data, "g", min = 2, sep = "; ")

  expect_equal(res$keep, "a")
  expect_equal(res$drop, "b [1]; c [1].")
})

test_that("fct_keep does not error when every level passes min", {
  data <- data.frame(g = c("a", "a", "b", "b"))

  res <- fct_keep(data, "g", min = 1, sep = "; ")

  expect_setequal(res$keep, c("a", "b"))
  expect_equal(as.character(res$drop), ".")
})

test_that("fct_keep does not error when no level passes min", {
  data <- data.frame(g = c("a", "a", "b"))

  res <- fct_keep(data, "g", min = 5, sep = "; ")

  expect_equal(res$keep, character(0))
  expect_match(res$drop, "a \\[2\\]")
  expect_match(res$drop, "b \\[1\\]")
})

test_that("add_label inserts the label before the exact variable, not a substring match", {
  d <- data.frame(
    age_group = factor(c("x", "y", "x", "y")),
    age = c(20, 30, 40, 50)
  )
  tbl <- gtsummary::tbl_summary(d)

  res <- add_label(tbl, name = "Demographics", levels = "age")

  label_index <- which(res$table_body$label == "Demographics")
  expect_length(label_index, 1)
  expect_equal(res$table_body$variable[label_index + 1], "age")
})

test_that("fct_str counts levels by decreasing frequency and capitalises when asked", {
  x <- c("a", "a", "b")

  expect_equal(as.character(fct_str(x, sep = "; ")), "A [2]; b [1].")
  expect_equal(
    as.character(fct_str(x, sep = "; ", cap = FALSE)),
    "a [2]; b [1]."
  )
})

test_that("all_dichotomous_uv keeps only two-level factor columns", {
  data <- data.frame(
    x = factor(c("a", "b", "a", "b")),
    y = factor(c("a", "b", "c", "a")),
    z = c(1, 2, 3, 4)
  )

  expect_equal(all_dichotomous_uv(data), "x")
  expect_equal(all_dichotomous_uv(data, "x", "z"), "x")
})

test_that("show_single_row recodes two-level factors to 0/1 and excludes the first column", {
  data <- data.frame(id = 1:4, sex = factor(c("f", "m", "f", "m")))

  res <- show_single_row(data)

  expect_equal(res$id, 1:4)
  expect_equal(res$sex, c(0, 1, 0, 1))
})

test_that("str_na_mv reports the count and share of rows with any missing value", {
  with_na <- data.frame(a = c(1, NA, 3), b = c("x", "y", NA))
  without_na <- data.frame(a = c(1, 2, 3), b = c("x", "y", "z"))

  expect_match(
    as.character(str_na_mv(with_na)),
    "^3 observations, 2 observations \\(66"
  )
  expect_match(as.character(str_na_mv(without_na)), "aucune observation")
})

.make_note_tbl <- \() {
  gtsummary::tbl_summary(
    data.frame(
      age = c(30, 40, 50, 60),
      sex = factor(c("F", "M", "F", "M"))
    ),
    include = c(age, sex)
  )
}

.note_labels <- \(x) {
  rows <- x$table_styling$footnote_body$rows[[1]]

  x$table_body$label[eval_tidy(rows, data = x$table_body)]
}

test_that("add_note aborts on a table already rendered by tbl_format", {
  g <- gt::gt(data.frame(label = c("Alpha", "Beta", "Gamma")))

  expect_error(
    add_note(g, vars = "age", note = "test note"),
    class = "rlang_error"
  )
})

test_that("add_note aborts when no targeting argument is provided", {
  expect_error(
    add_note(.make_note_tbl(), note = "test note"),
    class = "rlang_error"
  )
})

test_that("add_note targets only the label row of the requested vars", {
  res <- add_note(.make_note_tbl(), vars = "sex", note = "test note")
  footnote <- res$table_styling$footnote_body

  expect_equal(nrow(footnote), 1)
  expect_equal(footnote$column, "label")
  expect_equal(footnote$footnote, "test note")
  expect_equal(.note_labels(res), "sex")
})

test_that("add_note targets the rows matching the requested levels", {
  res <- add_note(.make_note_tbl(), levels = "F", note = "test note")

  expect_equal(nrow(res$table_styling$footnote_body), 1)
  expect_equal(.note_labels(res), "F")
})

test_that("add_note targets rows selected by a data-mask expression", {
  res <- add_note(
    .make_note_tbl(),
    rows = label == "M",
    note = "test note"
  )

  expect_equal(nrow(res$table_styling$footnote_body), 1)
  expect_equal(.note_labels(res), "M")
})

test_that("add_note resolves a rows expression against the caller's environment", {
  wrapper <- \() {
    kept <- "F"

    add_note(.make_note_tbl(), rows = label %in% kept, note = "test note")
  }

  res <- wrapper()

  expect_equal(nrow(res$table_styling$footnote_body), 1)
  expect_equal(.note_labels(res), "F")
})

test_that("add_note footnotes the multivariable p-value column label", {
  res <- .make_note_tbl() |>
    modify_table_body(\(.b) dplyr::mutate(.b, p.value_2 = 0.03)) |>
    add_note(pvalue_mv = 1, note = "test note")

  footnote <- res$table_styling$footnote_header |>
    dplyr::filter(column == "p.value_2")

  expect_equal(nrow(footnote), 1)
  expect_equal(footnote$footnote, "test note")
})

test_that("add_note survives tbl_format on both output formats", {
  set_opts()

  build <- \() add_note(.make_note_tbl(), vars = "sex", note = "test note")

  withr::with_options(list(hebstr.docx = FALSE), {
    expect_s3_class(tbl_format(build()), "gt_tbl")
  })

  withr::with_options(list(hebstr.docx = TRUE), {
    expect_s3_class(tbl_format(build()), "flextable")
  })
})

.make_ref_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 30),
    grp = factor(
      rep(c("low", "mid", "high"), 20),
      levels = c("low", "mid", "high")
    )
  )
  mod <- glm(y ~ grp, data = df, family = binomial())
  gtsummary::tbl_regression(mod, exponentiate = TRUE)
}

test_that("add_ref_label sets the missing symbol on the reference row estimate columns", {
  tbl <- .make_ref_tbl()

  res <- add_ref_label(tbl)

  expect_s3_class(res, "gtsummary")
  added <- res$table_styling$fmt_missing[
    res$table_styling$fmt_missing$symbol == "Reference",
  ]
  expect_setequal(added$column, c("estimate", "conf.low", "conf.high"))
})

test_that("add_ref_label honors a custom reference label", {
  tbl <- .make_ref_tbl()

  res <- add_ref_label(tbl, label = "Ref.")

  fm <- res$table_styling$fmt_missing
  expect_contains(fm$symbol, "Ref.")
  expect_false("Reference" %in% fm$symbol)
  expect_setequal(
    fm$column[fm$symbol == "Ref."],
    c("estimate", "conf.low", "conf.high")
  )
})
