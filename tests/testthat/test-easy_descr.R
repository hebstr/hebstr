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

test_that("easy_descr keeps single-term parametric classification", {
  data <- data.frame(
    mpg = c(1.2, 3.4, 5.6, 7.8),
    score = c(1.1, 2.2, 3.3, 4.4)
  )

  res <- suppressMessages(easy_descr(data, parametric = "mpg"))

  expect_equal(res$qt$vars$parametric, "mpg")
  expect_equal(res$qt$vars$nonparametric, "score")
})
