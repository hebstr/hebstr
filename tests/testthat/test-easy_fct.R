test_that("character branch recodes and relevels via ...", {
  df <- tibble::tibble(x = c("A", "B", "C", "A"))
  res <- easy_fct(df, x, "Alpha" = "A", "Beta" = "B", "Charlie" = "C")

  expect_s3_class(res$x, "factor")
  expect_equal(as.character(res$x), c("Alpha", "Beta", "Charlie", "Alpha"))
  expect_equal(levels(res$x), c("Alpha", "Beta", "Charlie"))
})

test_that("character branch recodes via .dots", {
  df <- tibble::tibble(x = c("A", "B", "C", "A"))
  mapping <- list("Alpha" = "A", "Beta" = "B", "Charlie" = "C")
  res <- easy_fct(df, x, .dots = mapping)

  expect_s3_class(res$x, "factor")
  expect_equal(as.character(res$x), c("Alpha", "Beta", "Charlie", "Alpha"))
  expect_equal(levels(res$x), c("Alpha", "Beta", "Charlie"))
})

test_that(".dots and ... can be combined", {
  df <- tibble::tibble(x = c("A", "B", "C"))
  extra <- list("Charlie" = "C")
  res <- easy_fct(df, x, "Alpha" = "A", "Beta" = "B", .dots = extra)

  expect_s3_class(res$x, "factor")
  expect_equal(as.character(res$x), c("Alpha", "Beta", "Charlie"))
  expect_equal(levels(res$x), c("Alpha", "Beta", "Charlie"))
})

test_that("character branch without ... converts to factor with default levels", {
  df <- tibble::tibble(x = c("B", "A", "C"))
  res <- easy_fct(df, x)

  expect_s3_class(res$x, "factor")
  expect_equal(as.character(res$x), c("B", "A", "C"))
})

test_that("numeric branch with inf/sup bins into 3 levels", {
  df <- tibble::tibble(age = c(10, 25, 50, 65, 80))
  res <- easy_fct(df, age, inf = 18, sup = 65)

  expect_s3_class(res$age, "factor")
  expect_equal(levels(res$age), c("<18", "[18-65)", ">=65"))
  expect_equal(
    as.character(res$age),
    c("<18", "[18-65)", "[18-65)", ">=65", ">=65")
  )
})

test_that("numeric branch with .btw bins into 4 levels", {
  df <- tibble::tibble(age = c(10, 25, 45, 65, 80))
  res <- easy_fct(df, age, inf = 18, sup = 65, .btw = 40)

  expect_s3_class(res$age, "factor")
  expect_equal(levels(res$age), c("<18", "[18-40)", "[40-65)", ">=65"))
})

test_that("numeric branch with cut bins into 2 levels", {
  df <- tibble::tibble(score = c(3, 7, 10, 15))
  res <- easy_fct(df, score, cut = 10)

  expect_s3_class(res$score, "factor")
  expect_equal(levels(res$score), c("<10", ">=10"))
  expect_equal(as.character(res$score), c("<10", "<10", ">=10", ">=10"))
})

test_that("numeric branch with cut handles negative values", {
  df <- tibble::tibble(val = c(-5, 0, 5))
  res <- easy_fct(df, val, cut = 0)

  expect_s3_class(res$val, "factor")
  expect_equal(as.character(res$val), c("<0", ">=0", ">=0"))
})

test_that(".name creates a new column", {
  df <- tibble::tibble(age = c(10, 50, 80))
  res <- easy_fct(df, age, inf = 18, sup = 65, .name = rlang::sym("age_grp"))

  expect_contains(names(res), c("age", "age_grp"))
  expect_type(res$age, "double")
  expect_s3_class(res$age_grp, "factor")
})

test_that("numeric branch errors when inf/sup missing", {
  df <- tibble::tibble(age = c(10, 50))

  expect_error(easy_fct(df, age), class = "rlang_error")
  expect_error(easy_fct(df, age, inf = 18), class = "rlang_error")
  expect_error(easy_fct(df, age, sup = 65), class = "rlang_error")
})
