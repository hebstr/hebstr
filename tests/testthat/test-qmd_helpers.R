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

test_that("glue_qmd() reports the class and length of a non-character input", {
  expect_error(glue_qmd(123), "<numeric> of length 1", class = "rlang_error")
  expect_error(glue_qmd(NULL), "<NULL> of length 0", class = "rlang_error")
  expect_error(glue_qmd(TRUE), "<logical> of length 1", class = "rlang_error")
})

test_that("glue_qmd() reports the length of a multi-element character vector", {
  expect_error(
    glue_qmd(c("a", "b")),
    "<character> of length 2",
    class = "rlang_error"
  )
})

test_that("gt_qmd() returns a gt object from a data.frame", {
  result <- gt_qmd(head(mtcars, 3))
  expect_s3_class(result, "gt_tbl")
})

test_that("gt_qmd() respects top_n argument", {
  result <- gt_qmd(mtcars, top_n = 2)
  expect_s3_class(result, "gt_tbl")
  expect_true(nrow(result[["_data"]]) < nrow(mtcars))
})

test_that("gt_qmd() sets the html id on the data.frame and gtsummary paths", {
  .id_of <- \(x) {
    opts <- x[["_options"]]
    opts$value[opts$parameter == "table_id"][[1]]
  }

  tbl <- suppressMessages(gtsummary::tbl_summary(
    head(mtcars, 4),
    include = mpg
  ))

  expect_equal(.id_of(gt_qmd(head(mtcars, 3), id = "tbl-a")), "tbl-a")
  expect_equal(.id_of(gt_qmd(tbl, id = "tbl-b")), "tbl-b")
})

test_that("gt_qmd() gives each table its own id by default", {
  .rendered_id <- \(x) {
    html <- gt::as_raw_html(x, inline_css = FALSE)
    match <- regmatches(html, regexpr('id="[^"]+"', html))
    sub('id="([^"]+)"', "\\1", match)
  }

  first <- .rendered_id(gt_qmd(head(mtcars, 2)))
  second <- .rendered_id(gt_qmd(head(mtcars, 2)))

  expect_length(first, 1)
  expect_false(identical(first, second))
})

test_that("gt_qmd() leaves the id unset on the top_n path", {
  result <- gt_qmd(mtcars, top_n = 2, id = "tbl-c")
  opts <- result[["_options"]]

  expect_true(is.na(opts$value[opts$parameter == "table_id"][[1]]))
})

test_that("gt_qmd() applies custom font and size", {
  result <- gt_qmd(head(iris, 2), font_family = "Arial", font_size = 20)
  opts <- result[["_options"]]
  font <- opts[opts$parameter == "table_font_names", "value"][[1]][[1]]
  size <- opts[opts$parameter == "table_font_size", "value"][[1]][[1]]
  expect_equal(font, "Arial")
  expect_equal(size, "20px")
})

test_that("gt_qmd() default font_family reads the centralised text font", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  result <- gt_qmd(head(mtcars, 3))
  opts_tbl <- result[["_options"]]
  font <- opts_tbl[opts_tbl$parameter == "table_font_names", "value"][[1]][[1]]

  expect_identical(font, "PinnedAlpha")
})

test_that("gt_qmd() reports the class of an invalid data input", {
  expect_error(
    gt_qmd("not a df"),
    "Object of class <character> supplied",
    class = "rlang_error"
  )
  expect_error(
    gt_qmd(42),
    "Object of class <numeric> supplied",
    class = "rlang_error"
  )
  expect_error(
    gt_qmd(list(a = 1)),
    "Object of class <list> supplied",
    class = "rlang_error"
  )
})

test_that("gt_qmd() names top_n as the offending argument", {
  expect_error(
    gt_qmd(mtcars, top_n = -1),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
  expect_error(
    gt_qmd(mtcars, top_n = "a"),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
  expect_error(
    gt_qmd(mtcars, top_n = c(1, 2)),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
})

test_that("gt_qmd() works with gtsummary objects", {
  tbl_sum <- gtsummary::trial[1:10, ] |> gtsummary::tbl_summary(include = age)
  result <- gt_qmd(tbl_sum)
  expect_s3_class(result, "gt_tbl")
})

test_that("include_code_file() returns a glue string with correct structure", {
  result <- include_code_file("script.R")
  expect_s3_class(result, "glue")
  expect_match(result, "include='script.R'")
  expect_match(result, "code-line-numbers='true'")
  expect_match(result, "\\.r")
})

test_that("include_code_file() uses custom lang", {
  result <- include_code_file("query.sql", lang = "sql")
  expect_match(result, "\\.sql")
  expect_match(result, "include='query.sql'")
})

test_that("include_code_file() names src as the offending argument", {
  expect_error(include_code_file(123), "`src` must be", class = "rlang_error")
  expect_error(include_code_file(NULL), "`src` must be", class = "rlang_error")
  expect_error(
    include_code_file(c("a", "b")),
    "`src` must be",
    class = "rlang_error"
  )
})

test_that("include_code_file() names lang, not src, when only lang is invalid", {
  expect_error(
    include_code_file("script.R", lang = 1),
    "`lang` must be",
    class = "rlang_error"
  )
  expect_error(
    include_code_file("script.R", lang = NULL),
    "`lang` must be",
    class = "rlang_error"
  )
})
