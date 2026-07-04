test_that("acro() builds the English dictionary by default", {
  withr::local_options(OutDec = ".")

  res <- acro()

  expect_equal(res$SD, "SD: standard deviation")
  expect_true("95%CI" %in% names(res))
})

test_that("acro() builds the French dictionary when OutDec is a comma", {
  withr::local_options(OutDec = ",")

  res <- acro()

  expect_equal(res$SD, "SD : écart-type")
  expect_true("IC95%" %in% names(res))
})

test_that("acro() lets a custom entry override a base entry", {
  withr::local_options(OutDec = ".")

  res <- acro(SD ~ "my custom sd")

  expect_equal(res$SD, "SD: my custom sd")
  expect_equal(res$IQR, "IQR: interquartile range")
})

test_that("acro(.auto = FALSE) keeps only the custom entries", {
  withr::local_options(OutDec = ".")

  res <- acro(BMI ~ "body mass index", .auto = FALSE)

  expect_named(res, "BMI")
})

test_that("acro_match tolerates unlabelled variables", {
  df <- data.frame(a = 1, b = 2)
  labelled::var_label(df$a) <- "Mean with SD"

  res <- acro_match(df, acro_list = acro(), acro_sep = "; ")

  expect_match(res, "standard deviation")
})

test_that("acro_match returns NULL when no variable is labelled", {
  df <- data.frame(a = 1, b = 2)

  expect_null(acro_match(df, acro_list = acro(), acro_sep = "; "))
})

test_that("acro_match aborts on vars absent from x", {
  df <- data.frame(a = 1)

  expect_error(
    acro_match(df, vars = c("a", "zz"), acro_list = acro(), acro_sep = "; "),
    "zz"
  )
})

test_that("acro_extract does not match an acronym as a substring of a larger word", {
  acro_list <- list("CI" = "confidence interval")

  expect_length(acro_extract("CIRCLE analysis", acro_list), 0)
})

test_that("acro_extract still matches an acronym bounded by non-word characters", {
  acro_list <- list("CI" = "confidence interval", "OR" = "odds ratio")

  expect_setequal(
    acro_extract("95% CI reported, OR estimated.", acro_list),
    c("CI", "OR")
  )
})

test_that("acro_extract escapes regex metacharacters in dictionary names", {
  acro_list <- list("p.value" = "significance threshold")

  expect_length(acro_extract("reported pXvalue here", acro_list), 0)
  expect_equal(acro_extract("reported p.value here", acro_list), "p.value")
})
