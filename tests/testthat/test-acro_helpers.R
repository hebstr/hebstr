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
