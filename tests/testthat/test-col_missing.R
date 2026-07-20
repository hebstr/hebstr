test_that("col_missing() adds a dm column carrying the joined missing counts", {
  local_opts()

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
  local_opts()

  res <- col_missing(.make_missing_tbl())

  header <- res$table_styling$header |>
    dplyr::filter(column == "dm")

  expect_equal(header$label, "**MD**")
  expect_equal(header$align, "center")
})

test_that("col_missing() localises the default header to French under a comma OutDec", {
  withr::local_options(OutDec = ",")
  local_opts()

  res <- col_missing(.make_missing_tbl())

  header <- res$table_styling$header |>
    dplyr::filter(column == "dm")

  expect_equal(header$label, "**DM**")
})

test_that("col_missing() drops the missing rows from the table body", {
  local_opts()

  res <- col_missing(.make_missing_tbl())

  expect_false(any(res$table_body$row_type == "missing"))
})

test_that("col_missing() honours a custom prefix, empty, and header", {
  local_opts()

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

test_that("col_missing() warns on a missing-free table carrying NA row types", {
  local_opts()

  tbl <- add_label(
    .make_complete_tbl(),
    name = "Group",
    levels = "age"
  )

  expect_true(anyNA(tbl$table_body$row_type))
  expect_warning(res <- col_missing(tbl), "No missing rows to collapse")
  expect_false("dm" %in% names(res$table_body))
})

test_that("col_missing() collapses missing rows in a table that also carries NA row types", {
  local_opts()

  tbl <- add_label(
    .make_missing_tbl(),
    name = "Group",
    levels = c("age", "sex")
  )

  expect_true(anyNA(tbl$table_body$row_type))
  expect_true("missing" %in% tbl$table_body$row_type)

  body <- col_missing(tbl)$table_body

  expect_match(
    body$dm[body$variable %in% "age" & body$row_type %in% "label"],
    "0/2"
  )
  expect_match(
    body$dm[body$variable %in% "sex" & body$row_type %in% "label"],
    "1/0"
  )
  expect_true(is.na(body$dm[is.na(body$row_type)]))
  expect_false("missing" %in% body$row_type)
})

test_that("col_missing() is idempotent: a second call warns and leaves the table unchanged", {
  local_opts()

  once <- col_missing(.make_missing_tbl())

  expect_warning(twice <- col_missing(once), "dm")
  expect_equal(twice$table_body, once$table_body)
})

test_that("col_missing() warns and returns the table unchanged when no missing rows exist", {
  local_opts()

  tbl <- .make_complete_tbl()

  expect_warning(res <- col_missing(tbl), "No missing rows to collapse")
  expect_false("dm" %in% names(res$table_body))
  expect_equal(res$table_body, tbl$table_body)
})
