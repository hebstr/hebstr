test_that("easy_view() propagates the centralised text font into the widget", {
  withr::defer(rm(list = "opts", envir = globalenv()))
  assign("opts", list(font = list(alpha = "PinnedAlpha")), envir = globalenv())

  view <- easy_view(head(mtcars))

  expect_true(any(grepl("PinnedAlpha", unlist(view), fixed = TRUE)))
})

test_that("easy_view() renders standalone when opts is absent", {
  if (exists("opts", envir = globalenv(), inherits = FALSE)) {
    rm(list = "opts", envir = globalenv())
  }

  expect_no_error(easy_view(head(mtcars)))
})
