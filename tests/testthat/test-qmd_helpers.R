test_that("glue_qmd() interpolates with << >> delimiters", {
  x <- "world"
  expect_equal(glue_qmd("hello <<x>>"), glue::as_glue("hello world"))
})

test_that("glue_qmd() leaves curly braces untouched", {
  result <- glue_qmd("```{r}")
  expect_equal(result, glue::as_glue("```{r}"))
})

test_that("glue_qmd() evaluates in the caller's environment", {
  wrapper <- \() {
    val <- 42
    glue_qmd("result: <<val>>")
  }
  expect_equal(wrapper(), glue::as_glue("result: 42"))
})

test_that("glue_qmd() errors on non-character input", {
  expect_error(glue_qmd(123), class = "rlang_error")
  expect_error(glue_qmd(NULL), class = "rlang_error")
  expect_error(glue_qmd(TRUE), class = "rlang_error")
})

test_that("glue_qmd() errors on multi-element character vector", {
  expect_error(glue_qmd(c("a", "b")), class = "rlang_error")
})

test_that("gt_qmd() returns a gt object from a data.frame", {
  result <- gt_qmd(head(mtcars, 3))
  expect_s3_class(result, "gt_tbl")
})

test_that("gt_qmd() works with top_n argument", {
  result <- gt_qmd(mtcars, top_n = 2)
  expect_s3_class(result, "gt_tbl")
})

test_that("gt_qmd() applies custom font and size", {
  result <- gt_qmd(head(iris, 2), font_family = "Arial", font_size = 20)
  opts <- result[["_options"]]
  font <- opts[opts$parameter == "table_font_names", "value"][[1]][[1]]
  size <- opts[opts$parameter == "table_font_size", "value"][[1]][[1]]
  expect_equal(font, "Arial")
  expect_equal(size, "20px")
})

test_that("gt_qmd() errors on invalid data input", {
  expect_error(gt_qmd("not a df"), class = "rlang_error")
  expect_error(gt_qmd(42), class = "rlang_error")
  expect_error(gt_qmd(list(a = 1)), class = "rlang_error")
})

test_that("gt_qmd() errors on invalid top_n", {
  expect_error(gt_qmd(mtcars, top_n = -1), class = "rlang_error")
  expect_error(gt_qmd(mtcars, top_n = "a"), class = "rlang_error")
  expect_error(gt_qmd(mtcars, top_n = c(1, 2)), class = "rlang_error")
})

test_that("gt_qmd() works with gtsummary objects", {
  skip_if_not_installed("gtsummary")
  tbl_sum <- gtsummary::trial[1:10, ] |> gtsummary::tbl_summary(include = age)
  result <- gt_qmd(tbl_sum)
  expect_s3_class(result, "gt_tbl")
})
