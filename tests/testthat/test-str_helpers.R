test_that("str_cap applies fun to the first character", {
  expect_equal(str_cap(toupper, "données"), "Données")
  expect_equal(str_cap(tolower, c("Mean", "Median")), c("mean", "median"))
})

test_that("str_cap treats a leading regex metacharacter literally", {
  expect_equal(
    str_cap(toupper, "(entre parenthèses)"),
    "(entre parenthèses)"
  )
  expect_equal(str_cap(toupper, "+repeated"), "+repeated")
  expect_equal(str_cap(\(x) "X", "[abc]"), "Xabc]")
})
