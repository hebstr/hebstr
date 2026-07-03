test_that("quanti.test.para returns a tidy ANOVA result with more than 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7, 15, 14, 16),
    g = rep(c("a", "b", "c"), each = 3)
  )

  res <- quanti.test.para(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_true("p.value" %in% names(res))
  expect_equal(nrow(res), 1)
  expect_equal(
    res$p.value,
    oneway.test(y ~ g, data = data, var.equal = TRUE)$p.value
  )
})

test_that("fct_other_str combines rare factor levels and character counts", {
  fct <- factor(c("Alpha", "Alpha", "Alpha", "Beta", "Gamma", "Other", "Other"))
  chr <- c("x y", "x y", "z")

  res <- fct_other_str(fct, chr, min = 2)

  expect_equal(as.character(res), "Beta (1), gamma (1), x y [2], z [1].")
})

test_that("fct_other_str forwards sep to the character counts", {
  fct <- factor(c("Alpha", "Alpha", "Alpha", "Beta", "Gamma", "Other", "Other"))
  chr <- c("x y", "x y", "z")

  res <- fct_other_str(fct, chr, min = 2, sep = " ; ")

  expect_equal(as.character(res), "Beta (1), gamma (1), x y [2] ; z [1].")
})

test_that("quanti.test.para still runs a t-test with 2 groups", {
  data <- data.frame(
    y = c(1, 3, 2, 8, 9, 7),
    g = rep(c("a", "b"), each = 3)
  )

  res <- quanti.test.para(data, "y", "g")

  expect_s3_class(res, "data.frame")
  expect_equal(
    res$p.value,
    t.test(y ~ g, data = data, var.equal = TRUE)$p.value
  )
})
