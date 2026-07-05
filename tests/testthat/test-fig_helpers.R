test_that("ggcount() aborts when var is not a column of data", {
  skip_if_not_installed("ggplot2")

  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  expect_error(ggcount(d, "nonexistent"), "var")
})

test_that("ggcount() builds a bar layer for a valid column", {
  skip_if_not_installed("ggplot2")

  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  built <- ggplot2::ggplot_build(ggcount(d, "class"))

  expect_equal(sort(built$data[[1]]$count), c(2, 3, 5))
})

test_that("ggcount() default family reads the centralised text font", {
  skip_if_not_installed("ggplot2")
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = globalenv())

  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  expect_identical(ggcount(d, "class")$theme$text$family, "PinnedAlpha")
})
