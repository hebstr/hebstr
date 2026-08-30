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
  fonts <- .cell_fonts(wb, c("E2", "A2"))

  expect_match(fonts[["E2"]], 'rgb="FFFF0000"', fixed = TRUE)
  expect_match(fonts[["E2"]], '<b val="1"/>', fixed = TRUE)
  expect_no_match(fonts[["A2"]], '<b val="1"/>', fixed = TRUE)
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
  expect_match(.cell_fills(wb, "A1")[["A1"]], "FFCCCCCC", fixed = TRUE)
  expect_match(.cell_fonts(wb, "A1")[["A1"]], '<sz val="11"/>', fixed = TRUE)
  expect_match(.cell_fonts(wb, "A2")[["A2"]], '<sz val="10"/>', fixed = TRUE)
  expect_match(.cell_xf(wb, "A2")[["A2"]], 'horizontal="left"', fixed = TRUE)
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

test_that("get_xlsx() caps the auto column width at max_width", {
  data <- data.frame(wide = strrep("a", 80), narrow = 1)
  attrs <- get_xlsx(list(s = data), max_width = 25)$worksheets[[1]]$cols_attr

  expect_match(attrs[[1]], 'width="25"', fixed = TRUE)
  expect_no_match(attrs[[2]], 'width="25"', fixed = TRUE)
})

test_that("get_xlsx() applies a distinct bold color to each named column", {
  wb <- get_xlsx(
    list(iris = head(iris, 5)),
    color = list(Sepal.Length = "#FF0000", Sepal.Width = "#0000FF")
  )
  fonts <- .cell_fonts(wb, c("A2", "B2", "C2"))

  expect_match(fonts[["A2"]], 'rgb="FFFF0000"', fixed = TRUE)
  expect_match(fonts[["B2"]], 'rgb="FF0000FF"', fixed = TRUE)
  expect_no_match(fonts[["C2"]], '<b val="1"/>', fixed = TRUE)
})

test_that("get_xlsx() borders every cell of the sheet identically", {
  wb <- get_xlsx(list(iris = head(iris, 6)))
  borders <- .cell_borders(
    wb,
    c("A1", "C1", "E1", "A2", "C4", "E4", "A7", "C7", "E7")
  )

  expect_length(unique(borders), 1L)
  expect_match(unique(borders), "FFC5C5C5", fixed = TRUE)
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

  expect_equal(unname(.cell_xf(wb, c("A5", "F1"))), c("", ""))
})

test_that("get_xlsx() bounds each sheet's border on its own column count", {
  wb <- get_xlsx(list(narrow = head(iris, 2)[, 1:2], wide = head(iris, 2)))

  expect_equal(unname(.cell_borders(wb, "E1", sheet = "narrow")), "")
  expect_match(
    .cell_borders(wb, "B1", sheet = "narrow"),
    "FFC5C5C5",
    fixed = TRUE
  )
  expect_match(
    .cell_borders(wb, "E1", sheet = "wide"),
    "FFC5C5C5",
    fixed = TRUE
  )
})

test_that("get_xlsx() styles a zero-row sheet without spilling below the header", {
  wb <- suppressWarnings(get_xlsx(list(iris = iris[0, ])))

  borders <- unique(.cell_borders(wb, c("A1", "E1")))
  header <- .cell_fonts(wb, c("A1", "E1"))

  expect_length(borders, 1L)
  expect_match(borders, "FFC5C5C5", fixed = TRUE)
  expect_equal(unname(.cell_borders(wb, "A2")), "")

  expect_match(header, '<b val="1"/>', fixed = TRUE)
  expect_match(header, '<sz val="9"/>', fixed = TRUE)
})

test_that("get_xlsx() keeps the highlight color off a zero-row sheet's header", {
  wb <- suppressWarnings(
    get_xlsx(list(iris = iris[0, ]), color = list(Species = "#FF0000"))
  )

  expect_no_match(
    .cell_fonts(wb, "E1")[["E1"]],
    'rgb="FFFF0000"',
    fixed = TRUE
  )
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

test_that("get_xlsx() errors on an unnamed color list", {
  expect_error(
    get_xlsx(list(iris = head(iris, 3)), color = list("#FF0000")),
    "`color` must be a fully named list"
  )
})

test_that("get_xlsx() errors on a partially named color list", {
  expect_error(
    get_xlsx(
      list(iris = head(iris, 3)),
      color = list(Species = "#FF0000", "#0000FF")
    ),
    "`color` must be a fully named list"
  )
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
  on_iris <- .cell_fonts(wb, c("A2", "B2", "C2"), sheet = "iris")
  on_mtcars <- .cell_fonts(wb, c("A2", "B2", "C2"), sheet = "mtcars")

  expect_match(on_iris[["A2"]], 'rgb="FFFF0000"', fixed = TRUE)
  expect_match(on_iris[["B2"]], 'rgb="FF0000FF"', fixed = TRUE)
  expect_no_match(on_iris[["C2"]], '<b val="1"/>', fixed = TRUE)

  expect_match(on_mtcars[["A2"]], 'rgb="FFFFFF00"', fixed = TRUE)
  expect_no_match(on_mtcars[["B2"]], '<b val="1"/>', fixed = TRUE)
  expect_match(on_mtcars[["C2"]], 'rgb="FFFA8000"', fixed = TRUE)
})

test_that("get_xlsx() aligns the columns a named halign names", {
  data <- head(iris, 3)
  wb <- get_xlsx(
    list(a = data),
    halign = list(Sepal.Width = "left", Petal.Width = "right")
  )

  expect_equal(
    unname(.sheet_halign(wb, data, row = 2)),
    c("center", "left", "center", "right", "center")
  )
})

test_that("get_xlsx() carries a named halign to the header and the last row", {
  data <- head(iris, 5)
  wb <- get_xlsx(list(a = data), halign = list(Species = "left"))

  expect_equal(.sheet_halign(wb, data, row = 1)[["Species"]], "left")
  expect_equal(
    .sheet_halign(wb, data, row = nrow(data) + 1)[["Species"]],
    "left"
  )
})

test_that("get_xlsx() ignores a halign naming a column the sheet does not carry", {
  data <- head(iris, 3)
  wb <- get_xlsx(list(a = data), halign = list(absent = "left"))

  expect_true(all(.sheet_halign(wb, data, row = 2) == "center"))
})

test_that("get_xlsx() aborts on an unnamed halign list", {
  expect_error(
    get_xlsx(list(a = head(iris, 3)), halign = list("left")),
    "must be a fully named list"
  )
})

test_that("get_xlsx() sizes a column on its header when the values are shorter", {
  data <- data.frame(a_long_header_name = 1:3, b = strrep("x", 30))
  attrs <- get_xlsx(list(s = data))$worksheets[[1]]$cols_attr
  width <- \(i) as.numeric(sub('.*width="([0-9.]+)".*', "\\1", attrs[[i]]))

  expect_gt(width(1), nchar("a_long_header_name") * 8 / 11)
  expect_lt(width(2), 30)
})
