test_that("get_xlsx() returns a wbWorkbook object", {
  wb <- get_xlsx(list(a = head(iris, 5)))
  expect_s3_class(wb, "wbWorkbook")
})

test_that("get_xlsx() creates one sheet per list element", {
  wb <- get_xlsx(list(a = head(iris, 5), b = head(mtcars, 5)))
  expect_equal(length(wb$get_sheet_names()), 2L)
})

test_that("get_xlsx() uses list names as sheet names", {
  wb <- get_xlsx(list(Sheet1 = head(iris, 3), Sheet2 = head(mtcars, 3)))
  expect_equal(unname(wb$get_sheet_names()), c("Sheet1", "Sheet2"))
})

test_that("get_xlsx() works with a single sheet", {
  wb <- get_xlsx(list(only = head(iris, 3)))
  expect_equal(unname(wb$get_sheet_names()), "only")
})

test_that("get_xlsx() accepts the color argument without error", {
  expect_no_error(
    get_xlsx(list(iris = head(iris, 5)), color = list(Species = "#FF0000"))
  )
})

test_that("get_xlsx() accepts styling arguments without error", {
  expect_no_error(
    get_xlsx(
      list(a = head(iris, 3)),
      font_size = 10,
      halign = "left",
      header_color = "#CCCCCC",
      border_color = "#000000",
      max_width = 40
    )
  )
})

test_that("get_xlsx() preserves row count and column names", {
  data <- head(iris, 5)
  wb <- get_xlsx(list(iris = data))
  result <- openxlsx2::wb_to_df(wb, sheet = "iris", col_names = TRUE)
  expect_equal(nrow(result), nrow(data))
  expect_equal(names(result), names(data))
})

test_that("get_xlsx() does not leak the openxlsx2.maxWidth option", {
  withr::local_options(openxlsx2.maxWidth = 20)
  get_xlsx(list(a = head(iris, 3)), max_width = 40)
  expect_equal(getOption("openxlsx2.maxWidth"), 20)
})

test_that("get_xlsx() accepts color with multiple columns", {
  expect_no_error(
    get_xlsx(
      list(iris = head(iris, 5)),
      color = list(Sepal.Length = "#FF0000", Sepal.Width = "#0000FF")
    )
  )
})

test_that("get_xlsx() errors on unnamed list", {
  expect_error(get_xlsx(list(iris, mtcars)), class = "rlang_error")
})

test_that("get_xlsx() errors on partially named list", {
  expect_error(get_xlsx(list(iris = iris, mtcars)), class = "rlang_error")
})

test_that("get_xlsx() ignores color entries for columns absent from a sheet", {
  expect_no_error(
    get_xlsx(
      list(iris = head(iris, 5), mtcars = head(mtcars, 5)),
      color = list(
        Sepal.Length = "#FF0000",
        Sepal.Width = "#0000FF",
        mpg = "#FFFF00",
        disp = "#FA8000"
      )
    )
  )
})
