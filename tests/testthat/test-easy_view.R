test_that("easy_view() propagates the centralised text font into the widget", {
  withr::defer(rm(list = "opts", envir = .hebstr))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = .hebstr)

  view <- easy_view(head(mtcars))

  expect_true(any(grepl("PinnedAlpha", unlist(view), fixed = TRUE)))
})

test_that("easy_view() renders standalone when opts is absent", {
  if (exists("opts", envir = .hebstr, inherits = FALSE)) {
    rm(list = "opts", envir = .hebstr)
  }

  expect_no_error(easy_view(head(mtcars)))
})

test_that("easy_view() assembles the descriptive data columns", {
  df <- tibble::tibble(
    bin_num = c(1, 1, 2, 2, 2),
    cont = c(10, 20, 30, 40, NA),
    grp = c("x", "y", "x", "z", "y")
  )

  d <- easy_view(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "bin_num"]), "bin")
  expect_equal(unname(d$type[d$variable == "cont"]), "num")

  expect_true(is.na(d$n_miss[d$variable == "bin_num"]))
  expect_equal(unname(d$n_miss[d$variable == "cont"]), 1)
  expect_equal(unname(d$p_miss[d$variable == "cont"]), "20.0%")

  expect_equal(d$range[d$variable == "cont"][[1]], c(10, 40))
  expect_equal(
    unname(d$q1_q2_q3[d$variable == "cont"][[1]]),
    c(17.5, 25.0, 32.5)
  )
  expect_null(d$q1_q2_q3[d$variable == "bin_num"][[1]])
})

test_that("easy_view() reports the type and levels of categorical columns", {
  df <- tibble::tibble(
    grp = factor(c("x", "y", "x", "z", "y")),
    chr = c("a", "bb", "a", "a", "bb"),
    visit = as.Date("2020-01-01") + c(0, 10, 5, 30, 2)
  )

  d <- easy_view(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "grp"]), "fct")
  expect_equal(d$levels[d$variable == "grp"][[1]], c("x", "y", "z"))
  expect_equal(unname(d$type[d$variable == "chr"]), "chr")
})

test_that("easy_view() reports the range of Date columns", {
  df <- tibble::tibble(
    grp = factor(c("x", "y", "x", "z", "y")),
    visit = as.Date("2020-01-01") + c(0, 10, 5, 30, 2)
  )

  d <- easy_view(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "visit"]), "date")
  expect_equal(
    d$range[d$variable == "visit"][[1]],
    as.Date(c("2020-01-01", "2020-01-31"))
  )
})
