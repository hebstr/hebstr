test_that("ggcount() aborts when var is not a column of data", {
  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  expect_error(ggcount(d, "nonexistent"), "var")
})

test_that("ggcount() builds a bar layer for a valid column", {
  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  built <- ggplot2::ggplot_build(ggcount(d, "class"))

  expect_equal(sort(built$data[[1]]$count), c(2, 3, 5))
})

test_that("ggcount() percentage layer drops levels below pct_min", {
  d <- data.frame(class = rep(c("a", "b", "rare"), c(50, 45, 2)))

  built <- ggplot2::ggplot_build(ggcount(d, "class"))

  expect_equal(sort(built$data[[2]]$count), c(2, 45, 50))
  expect_equal(sort(built$data[[3]]$count), c(45, 50))
  expect_setequal(built$data[[3]]$label, c("51.5%", "46.4%"))
})

test_that("ggcount() default family reads the centralised text font", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  d <- data.frame(class = rep(c("a", "b", "c"), c(5, 3, 2)))

  expect_identical(ggcount(d, "class")$theme$text$family, "PinnedAlpha")
})
