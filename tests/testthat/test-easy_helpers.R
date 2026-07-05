test_that("easy_cut aborts when incr = FALSE and values is missing", {
  df <- data.frame(age = c(10, 25, 40, 55, 70))

  expect_error(easy_cut(df, age), "values")
})

test_that("easy_cut auto-generates labels when labels is NULL", {
  df <- data.frame(age = c(10, 25, 40, 55, 70))

  res <- easy_cut(df, age, values = c(20, 50))

  expect_equal(nlevels(res$age_cat), 3)
  expect_true(res$age_cat[2] == res$age_cat[3])
  expect_true(res$age_cat[4] == res$age_cat[5])
  expect_false(res$age_cat[1] == res$age_cat[2])
  expect_false(res$age_cat[3] == res$age_cat[4])
})

test_that("easy_cut places a value exactly on a break into the upper bin", {
  df <- data.frame(age = c(19, 20, 49, 50))

  res <- easy_cut(df, age, values = c(20, 50))

  expect_equal(as.integer(res$age_cat), c(1, 2, 2, 3))
})

test_that("easy_cut in incr mode returns sequential bin indices", {
  df <- data.frame(age = c(5, 15, 25, 35))

  res <- easy_cut(df, age, incr = TRUE, from = 0, to = 40, by = 10)

  expect_true("age_incr" %in% names(res))
  expect_equal(res$age_incr, c(1, 2, 3, 4))
})

test_that("easy_cut drops and renames the source column when drop = TRUE", {
  df <- data.frame(age = c(5, 15, 25, 35))

  res <- easy_cut(df, age, values = c(20), drop = TRUE)

  expect_named(res, "age")
  expect_s3_class(res$age, "factor")
})

test_that("flow_filter reports cumulative counts at each filtering step", {
  df <- data.frame(x = 1:10, g = rep(c("a", "b"), 5))

  res <- flow_filter(df, x > 3, g == "a")

  expect_length(res$flow, 3)
  expect_match(res$flow[[1]], "^10 ")
  expect_match(res$flow[[2]], "^7 ")
  expect_match(res$flow[[3]], "^3 ")
  expect_equal(nrow(res$data), 3)
})

test_that("logit_lty honors the breaks argument", {
  df <- data.frame(
    xv = seq_len(200),
    yv = factor(rep(c("no", "yes"), 100))
  )

  res <- logit_lty(df, yv, xv, breaks = 4)

  expect_equal(nrow(res$data), 4)
})

test_that("logit_lty defaults to 40 breaks", {
  df <- data.frame(
    xv = seq_len(200),
    yv = factor(rep(c("no", "yes"), 100))
  )

  res <- logit_lty(df, yv, xv)

  expect_equal(nrow(res$data), 40)
})

test_that("easy_boot returns the bootstrap list without assigning to globalenv", {
  withr::defer(
    if (exists("boot", envir = globalenv(), inherits = FALSE)) {
      rm(list = "boot", envir = globalenv())
    }
  )
  if (exists("boot", envir = globalenv(), inherits = FALSE)) {
    rm(list = "boot", envir = globalenv())
  }

  df <- data.frame(x = seq_len(30), y = seq_len(30) + rnorm(30))
  lm_stub <- \(fit_fun, data, ...) lm(y ~ x, data = data)

  res <- suppressWarnings(easy_boot(df, times = 5, method = lm_stub))

  expect_false(exists("boot", envir = globalenv(), inherits = FALSE))
  expect_named(res, c("estimate", "fitted"))
  expect_named(res$estimate, c("data", "int"))
  expect_s3_class(res$estimate$int, "data.frame")
  expect_contains(names(res$estimate$int), "term")
})

test_that("easy_ano hashes only exact-name matches, not substring matches", {
  df <- data.frame(
    id = c("a", "b"),
    patient_id = c("p1", "p2"),
    idade = c(1, 2)
  )

  res <- easy_ano(df, to_hash = "id")

  expect_false(any(res$id %in% df$id))
  expect_equal(res$patient_id, df$patient_id)
  expect_equal(res$idade, df$idade)
})

test_that("easy_ano hides only exact-name matches", {
  df <- data.frame(id = c("a", "b"), idade = c(1, 2))

  res <- easy_ano(df, to_hide = "id")

  expect_equal(res$id, c("---", "---"))
  expect_equal(res$idade, df$idade)
})

test_that("easy_ano aborts when a pattern matches no column", {
  df <- data.frame(id = "a")

  expect_error(easy_ano(df, to_hash = "idd"), "idd")
  expect_error(easy_ano(df, to_hide = "identifiant"), "identifiant")
})

test_that("easy_ano hashes are stable across group sizes", {
  df1 <- data.frame(id = c("A", "B"))
  df2 <- data.frame(id = c("A", "A", "B"))

  h1 <- easy_ano(df1, to_hash = "id")$id
  h2 <- easy_ano(df2, to_hash = "id")$id

  expect_equal(h1[1], h2[1])
  expect_equal(h2[1], h2[2])
  expect_equal(h1[2], h2[3])
})

test_that("easy_ano accepts explicit regex for column families", {
  df <- data.frame(id_pat = "a", id_visit = "b", other = 1)

  res <- easy_ano(df, to_hash = "id_.*")

  expect_false(res$id_pat == "a")
  expect_false(res$id_visit == "b")
  expect_equal(res$other, 1)
})

test_that("easy_ano hash_salt changes the hashes", {
  df <- data.frame(id = c("A", "B"))

  h_plain <- easy_ano(df, to_hash = "id")$id
  h_salted <- easy_ano(df, to_hash = "id", hash_salt = "s3cret")$id

  expect_false(any(h_plain == h_salted))
})

test_that("easy_ano keeps 16 hash characters by default", {
  df <- data.frame(id = "A")

  expect_equal(nchar(easy_ano(df, to_hash = "id")$id), 16)
})

test_that("p_shortenr returns an ungrouped tibble", {
  df <- data.frame(grp = c("a", "a", "b"), p.value = c(0.0005, 0.5, 0.02))

  res <- p_shortenr(df)

  expect_false(inherits(res, "rowwise_df"))
  expect_equal(as.character(res$p.value), c("<0.001", "0.5", "0.02"))
})

test_that("flow_filter collects a lazy dbplyr tbl before computing counts", {
  skip_if_not_installed("dbplyr")

  lf <- dbplyr::lazy_frame(x = 1:5)
  local_mocked_bindings(collect = \(data, ...) data.frame(x = 1:5))

  result <- flow_filter(lf, x > 2)

  expect_false(any(grepl("NA", result$flow)))
})

test_that("pct_min aborts when .var is not a column of data", {
  df <- data.frame(g = c("a", "a", "b"))

  expect_error(pct_min(df, "absent", 0.5), "is not a column")
})

test_that("pct_min keeps only levels at or above the relative-frequency threshold", {
  df <- data.frame(g = c("a", "a", "a", "a", "b"))

  res <- pct_min(df, "g", 0.5)

  expect_equal(nrow(res), 4)
  expect_setequal(res$g, "a")
})

test_that("pca_var_extract leaves variable names unchanged by default", {
  res <- pca_var_extract(prcomp(mtcars, scale = TRUE))

  expect_contains(res$coord$column, c("mpg", "cyl", "disp"))
})

test_that("pca_var_extract strips the supplied pattern from variable names", {
  pca <- prcomp(mtcars, scale = TRUE)

  plain <- pca_var_extract(pca)
  stripped <- pca_var_extract(pca, strip = "mp")

  expect_contains(stripped$coord$column, "g")
  expect_false("mpg" %in% stripped$coord$column)
  expect_equal(stripped$weight, plain$weight)
})
