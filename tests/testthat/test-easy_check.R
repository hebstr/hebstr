test_that("easy_check() reports one row per flagged check", {
  out <- easy_check(
    .make_check_df(),
    .with = list(site, n_total = n_first + n_second),
    no_treatment = n_total == 0,
    many_first = n_first > 2
  )

  expect_named(out, c("rowname", "check", "site", "n_total"))
  expect_equal(out$rowname, c(2, 3))
  expect_equal(out$check, c("many_first", "no_treatment"))
  expect_equal(out$n_total, c(3, 0))
})

test_that("easy_check() defaults its identifier to rowname", {
  df <- .make_check_df()

  expect_equal(
    easy_check(df, no_first = n_first == 0),
    easy_check(df, .id = "rowname", no_first = n_first == 0)
  )
})

test_that("easy_check() carries the columns of .with in declaration order", {
  out <- easy_check(
    .make_check_df(),
    .with = list(n_total = n_first + n_second, site),
    no_treatment = n_total == 0
  )

  expect_named(out, c("rowname", "check", "n_total", "site"))
})

test_that("easy_check() lets a check read a column computed by .with", {
  out <- easy_check(
    .make_check_df(),
    .with = list(n_total = n_first + n_second),
    no_treatment = n_total == 0
  )

  expect_equal(out$rowname, 3)
})

test_that("easy_check() drops undecidable checks by default", {
  out <- easy_check(.make_check_df(), end_before_start = end < start)

  expect_equal(out$rowname, 1)
})

test_that("easy_check(.na = 'flag') tells a failure from an unknown", {
  out <- easy_check(
    .make_check_df(),
    end_before_start = end < start,
    .na = "flag"
  )

  expect_named(out, c("rowname", "check", "status"))
  expect_equal(out$rowname, c(1, 2, 4))
  expect_equal(out$status, c("fail", "unknown", "unknown"))
})

test_that("easy_check() accepts a repeated identifier", {
  df <- .make_check_df()
  df$rowname <- c(1, 1, 2, 2)

  out <- easy_check(df, no_first = n_first == 0)

  expect_equal(out$rowname, c(1, 2))
})

test_that("easy_check() evaluates the checks group by group", {
  df <- .make_check_df()

  grouped <- easy_check(
    dplyr::group_by(df, site),
    at_site_max = n_first == max(n_first),
    .with = list(site)
  )

  expect_equal(grouped$rowname, c(1, 2, 3))
  expect_false(dplyr::is_grouped_df(grouped))

  expect_equal(
    easy_check(df, at_max = n_first == max(n_first))$rowname,
    2
  )
})

test_that("easy_check() returns an empty report when nothing is flagged", {
  out <- easy_check(.make_check_df(), never = n_first < 0)

  expect_equal(nrow(out), 0)
  expect_named(out, c("rowname", "check"))
})

test_that("easy_check() rejects an identifier it cannot find", {
  expect_error(
    easy_check(.make_check_df(), .id = "nope", no_first = n_first == 0),
    "must name a column"
  )
})

test_that("easy_check() rejects a check that is not logical", {
  expect_error(
    easy_check(.make_check_df(), total = n_first + n_second),
    "must evaluate to a logical vector"
  )
})

test_that("easy_check() rejects an unnamed check", {
  expect_error(
    easy_check(.make_check_df(), n_first == 0),
    "must be named"
  )
})

test_that("easy_check() rejects a check named after a carried column", {
  expect_error(
    easy_check(.make_check_df(), .with = list(site), site = n_first == 0),
    "cannot be named"
  )
})

test_that("easy_check() rejects a computed element of .with left unnamed", {
  expect_error(
    easy_check(
      .make_check_df(),
      .with = list(n_first + n_second),
      no_first = n_first == 0
    ),
    "Positions without a name"
  )
})

test_that("easy_check() rejects a bare element of .with that is not a column", {
  expect_error(
    easy_check(
      .make_check_df(),
      .with = list(nope),
      no_first = n_first == 0
    ),
    "must exist in"
  )
})

test_that("easy_check() rejects a .with that is not a list call", {
  expect_error(
    easy_check(
      .make_check_df(),
      .with = c(site),
      no_first = n_first == 0
    ),
    "must be a call to"
  )
})

test_that("easy_check() rejects an out-of-range .na", {
  expect_error(
    easy_check(.make_check_df(), no_first = n_first == 0, .na = "keep"),
    '"drop" or "flag"'
  )
})

test_that("easy_check() rejects a .name already carried", {
  expect_error(
    easy_check(
      .make_check_df(),
      .with = list(site),
      no_first = n_first == 0,
      .name = "site"
    ),
    "must not be a carried column"
  )
})

test_that("easy_check() renames its check column on request", {
  out <- easy_check(
    .make_check_df(),
    no_first = n_first == 0,
    .name = "anomalie"
  )

  expect_named(out, c("rowname", "anomalie"))
})
