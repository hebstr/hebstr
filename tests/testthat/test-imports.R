test_that("runtime dependencies resolve from the package namespace", {
  imports <- parent.env(asNamespace("hebstr"))

  expect_true(exists("as_factor", envir = imports, inherits = FALSE))
  expect_true(exists("remove_abbreviation", envir = imports, inherits = FALSE))
})
