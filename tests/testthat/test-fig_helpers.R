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
