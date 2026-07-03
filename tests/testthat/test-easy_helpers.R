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
