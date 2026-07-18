.make_missing_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = c(seq_len(18), NA, NA),
    sex = factor(c(rep("m", 9), NA, rep(c("m", "f"), 5)))
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = c(age, sex))
  )
}

.make_complete_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = seq_len(20)
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = age)
  )
}

test_that("col_missing() adds a dm column carrying the joined missing counts", {
  res <- col_missing(.make_missing_tbl())

  body <- res$table_body

  expect_true("dm" %in% names(body))
  expect_match(
    body$dm[body$variable == "age" & body$row_type == "label"],
    "0/2"
  )
  expect_match(
    body$dm[body$variable == "sex" & body$row_type == "label"],
    "1/0"
  )
})

test_that("col_missing() sets the dm column header and alignment", {
  res <- col_missing(.make_missing_tbl())

  header <- res$table_styling$header |>
    dplyr::filter(column == "dm")

  expect_equal(header$label, "**DM**")
  expect_equal(header$align, "center")
})

test_that("col_missing() drops the missing rows from the table body", {
  res <- col_missing(.make_missing_tbl())

  expect_false(any(res$table_body$row_type == "missing"))
})

test_that("col_missing() honours a custom prefix, empty, and header", {
  res <- col_missing(
    .make_missing_tbl(),
    prefix = "n =",
    header = "**Manquants**"
  )

  body <- res$table_body

  expect_match(
    body$dm[body$variable == "age" & body$row_type == "label"],
    "^n ="
  )
  expect_equal(
    res$table_styling$header$label[res$table_styling$header$column == "dm"],
    "**Manquants**"
  )
})

test_that("col_missing() is idempotent: a second call warns and leaves the table unchanged", {
  once <- col_missing(.make_missing_tbl())

  expect_warning(twice <- col_missing(once), "dm")
  expect_equal(twice$table_body, once$table_body)
})

test_that("col_missing() warns and returns the table unchanged when no missing rows exist", {
  tbl <- .make_complete_tbl()

  expect_warning(res <- col_missing(tbl), "missing")
  expect_false("dm" %in% names(res$table_body))
  expect_equal(res$table_body, tbl$table_body)
})
