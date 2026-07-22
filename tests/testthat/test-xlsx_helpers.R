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

test_that("get_xlsx() applies the requested bold font color to the target column", {
  wb <- get_xlsx(list(iris = head(iris, 5)), color = list(Species = "#FF0000"))
  expect_match(.bold_fonts(wb), 'rgb="FFFF0000"', all = FALSE, fixed = TRUE)
})

test_that("get_xlsx() applies the header fill color under a full styling bundle", {
  wb <- get_xlsx(
    list(a = head(iris, 3)),
    font_size = 10,
    halign = "left",
    header_color = "#CCCCCC",
    border_color = "#000000",
    max_width = 40
  )
  expect_match(
    wb$styles_mgr$styles$fills,
    "FFCCCCCC",
    all = FALSE,
    fixed = TRUE
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

test_that("get_xlsx() applies a distinct bold color to each named column", {
  wb <- get_xlsx(
    list(iris = head(iris, 5)),
    color = list(Sepal.Length = "#FF0000", Sepal.Width = "#0000FF")
  )
  fonts <- .bold_fonts(wb)
  expect_match(fonts, 'rgb="FFFF0000"', all = FALSE, fixed = TRUE)
  expect_match(fonts, 'rgb="FF0000FF"', all = FALSE, fixed = TRUE)
})

test_that("get_xlsx() borders every cell of the sheet identically", {
  wb <- get_xlsx(list(iris = head(iris, 6)))
  borders <- .cell_borders(
    wb,
    c("A1", "C1", "E1", "A2", "C4", "E4", "A7", "C7", "E7")
  )

  expect_length(unique(borders), 1L)
  expect_match(unique(borders), "FF999999", fixed = TRUE)
})

test_that("get_xlsx() borders follow border_color and border_type", {
  wb <- get_xlsx(
    list(iris = head(iris, 4)),
    border_color = "#000000",
    border_type = "medium"
  )
  borders <- unique(.cell_borders(wb, c("A1", "C3", "E5")))

  expect_length(borders, 1L)
  expect_match(borders, "FF000000", fixed = TRUE)
  expect_match(borders, 'style="medium"', fixed = TRUE)
})

test_that("get_xlsx() leaves cells outside the table unstyled", {
  wb <- get_xlsx(list(iris = head(iris, 3)))

  expect_equal(unname(.cell_borders(wb, c("A5", "F1"))), c("", ""))
})

test_that("get_xlsx() bounds each sheet's border on its own column count", {
  wb <- get_xlsx(list(narrow = head(iris, 2)[, 1:2], wide = head(iris, 2)))

  expect_equal(unname(.cell_borders(wb, "E1", sheet = "narrow")), "")
  expect_match(
    .cell_borders(wb, "B1", sheet = "narrow"),
    "FF999999",
    fixed = TRUE
  )
  expect_match(
    .cell_borders(wb, "E1", sheet = "wide"),
    "FF999999",
    fixed = TRUE
  )
})

test_that("get_xlsx() styles a zero-row sheet without spilling below the header", {
  wb <- suppressWarnings(get_xlsx(list(iris = iris[0, ])))

  borders <- unique(.cell_borders(wb, c("A1", "E1")))

  expect_length(borders, 1L)
  expect_match(borders, "FF999999", fixed = TRUE)
  expect_equal(unname(.cell_borders(wb, "A2")), "")
})

test_that("get_xlsx() preserves the number format of every column", {
  dates <- as.Date("2024-01-01") + 0:2
  roundtrip <- \(data) {
    file <- withr::local_tempfile(fileext = ".xlsx")
    openxlsx2::wb_save(get_xlsx(list(s = data)), file)
    openxlsx2::read_xlsx(file)
  }

  first <- roundtrip(data.frame(when = dates, n = 1:3))
  last <- roundtrip(data.frame(n = 1:3, when = dates))

  expect_equal(first$when, dates)
  expect_equal(first$n, c(1, 2, 3))
  expect_equal(last$when, dates)
  expect_equal(last$n, c(1, 2, 3))
})

test_that("get_xlsx() errors on unnamed list", {
  expect_error(get_xlsx(list(iris, mtcars)), class = "rlang_error")
})

test_that("get_xlsx() errors on partially named list", {
  expect_error(get_xlsx(list(iris = iris, mtcars)), class = "rlang_error")
})

test_that("get_xlsx() errors on a list carrying an NA name", {
  sheets <- list(iris, mtcars)
  names(sheets) <- c("iris", NA)

  expect_error(get_xlsx(sheets), class = "rlang_error")
})

test_that("get_xlsx() applies each color only to sheets that contain the column", {
  wb <- get_xlsx(
    list(iris = head(iris, 5), mtcars = head(mtcars, 5)),
    color = list(
      Sepal.Length = "#FF0000",
      Sepal.Width = "#0000FF",
      mpg = "#FFFF00",
      disp = "#FA8000"
    )
  )
  fonts <- .bold_fonts(wb)
  expect_match(fonts, 'rgb="FFFF0000"', all = FALSE, fixed = TRUE)
  expect_match(fonts, 'rgb="FF0000FF"', all = FALSE, fixed = TRUE)
  expect_match(fonts, 'rgb="FFFFFF00"', all = FALSE, fixed = TRUE)
  expect_match(fonts, 'rgb="FFFA8000"', all = FALSE, fixed = TRUE)
})
