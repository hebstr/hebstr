test_that("easy_descr bounds every alternation term of parametric", {
  data <- data.frame(
    mpg = c(1.2, 3.4, 5.6, 7.8),
    xhpx = c(2.3, 4.5, 6.7, 8.9),
    hp = c(10, 20, 30, 40),
    score = c(1.1, 2.2, 3.3, 4.4)
  )

  res <- suppressMessages(easy_descr(data, parametric = "mpg|hp|disp"))

  expect_setequal(res$qt$vars$parametric, c("mpg", "hp"))
  expect_setequal(res$qt$vars$nonparametric, c("xhpx", "score"))
})

test_that("easy_descr classifies a mixed data frame into every category", {
  data <- data.frame(
    age = c(20, 30, 40, 50, 60),
    score = c(1.1, 2.2, 3.3, 4.4, 5.5),
    sex = c("m", "f", "m", "f", "m"),
    smoker = c(0, 1, 0, 1, 0),
    visit = as.Date("2020-01-01") + 0:4
  )

  res <- suppressMessages(easy_descr(data))

  expect_setequal(res$qt$vars$total, c("age", "score"))
  expect_equal(res$ql$vars, "sex")
  expect_equal(res$bin$vars, "smoker")
  expect_equal(res$date$vars, "visit")
})

test_that("easy_descr treats every continuous variable as parametric with 'all_continuous'", {
  data <- data.frame(
    age = c(20, 30, 40, 50, 60),
    score = c(1.1, 2.2, 3.3, 4.4, 5.5)
  )

  res <- suppressMessages(easy_descr(data, parametric = "all_continuous"))

  expect_setequal(res$qt$vars$parametric, c("age", "score"))
  expect_null(res$qt$vars$nonparametric)
})

test_that("easy_descr uses French statistic labels when OutDec is a comma", {
  withr::local_options(OutDec = ",")
  data <- data.frame(age = c(20, 30, 40, 50, 60))

  res <- suppressMessages(easy_descr(data))

  expect_match(names(res$qt$stat$median), "Médiane")
  expect_match(names(res$qt$stat$mean), "Moyenne")
})

test_that("easy_descr keeps single-term parametric classification", {
  data <- data.frame(
    mpg = c(1.2, 3.4, 5.6, 7.8),
    score = c(1.1, 2.2, 3.3, 4.4)
  )

  res <- suppressMessages(easy_descr(data, parametric = "mpg"))

  expect_equal(res$qt$vars$parametric, "mpg")
  expect_equal(res$qt$vars$nonparametric, "score")
})
