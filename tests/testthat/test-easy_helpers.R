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

  expect_contains(names(res), "age_incr")
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

test_that("logit_lty aborts when the outcome is not a 2-level factor", {
  df <- data.frame(xv = seq_len(10), yv = rep(c(0, 1), 5))

  expect_error(logit_lty(df, yv, xv), "2-level factor")
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

test_that("p_shortenr does not prefix < on values above the threshold that round to it", {
  df <- data.frame(p.value = c(0.0011, 0.0013))

  res <- p_shortenr(df)

  expect_equal(as.character(res$p.value), c("0.001", "0.001"))
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


test_that("p_picking keeps only variables below the p-value threshold", {
  d <- data.frame(
    y = 1:12,
    x1 = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 7.2, 7.8, 9.1, 9.9, 11.2, 11.8),
    x2 = rep(c(0, 1), 6),
    x3 = c(5, 5, 5, 4, 6, 5, 5, 6, 4, 5, 5, 5)
  )

  picked <- p_picking(d, "lm", "y", c("x1", "x2", "x3"), 0.05)

  expect_equal(picked, "x1")
})

test_that("p_picking drops the response even when it is listed in vars", {
  d <- data.frame(
    y = 1:12,
    x1 = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 7.2, 7.8, 9.1, 9.9, 11.2, 11.8),
    x2 = rep(c(0, 1), 6)
  )

  picked <- p_picking(d, "lm", "y", c("y", "x1", "x2"), 0.05)

  expect_equal(picked, "x1")
})

test_that("p_picking returns an empty character vector when nothing qualifies", {
  d <- data.frame(
    y = 1:12,
    x1 = c(1.1, 1.9, 3.2, 3.8, 5.1, 5.9, 7.2, 7.8, 9.1, 9.9, 11.2, 11.8),
    x2 = rep(c(0, 1), 6)
  )

  picked <- p_picking(d, "lm", "y", c("x1", "x2"), 0)

  expect_type(picked, "character")
  expect_length(picked, 0)
})


test_that("cooksd returns the augmented data, thresholds, outliers and plot", {
  d <- data.frame(x = 1:15, y = c(11, 2:14, 55))

  res <- cooksd(lm(y ~ x, d))

  expect_named(res, c("data", "limit", "obs", "plot"))
  expect_false("outliers" %in% names(res))
  expect_contains(names(res$data), c("id", ".cooksd"))
  expect_equal(nrow(res$data), 15)
  expect_s3_class(res$plot, "ggplot")
})

test_that("cooksd derives its thresholds from the outlier factors and sample size", {
  d <- data.frame(x = 1:15, y = c(11, 2:14, 55))

  res <- cooksd(lm(y ~ x, d))
  custom <- cooksd(lm(y ~ x, d), limit_inf_num = 1, limit_sup_num = 100)

  expect_equal(res$limit, c(inf = 4 / 15, sup = 25 / 15))
  expect_equal(custom$limit, c(inf = 1 / 15, sup = 100 / 15))
})

test_that("cooksd flags more observations at the inf threshold than at the sup threshold", {
  d <- data.frame(x = 1:15, y = c(11, 2:14, 55))

  res <- cooksd(lm(y ~ x, d))

  expect_setequal(res$obs$inf$id, c("1", "15"))
  expect_setequal(res$obs$sup$id, "15")
  expect_contains(res$obs$inf$id, res$obs$sup$id)
})

test_that("cooksd reports no outliers on a well-behaved model", {
  d <- data.frame(x = 1:15, y = 1:15 + rep(c(0.1, -0.1), length.out = 15))

  res <- cooksd(lm(y ~ x, d))

  expect_length(res$obs$inf$id, 0)
  expect_length(res$obs$sup$id, 0)
})


test_that("easy_replace builds a named vector of HTML-wrapping regex patterns", {
  res <- easy_replace("age", "sex")

  expect_type(res, "character")
  expect_length(res, 3)
  expect_named(
    res,
    c("<p>.*(age).*</p>", "<p>.*(sex).*</p>", "(\n*</>)+\n*")
  )
  expect_equal(unname(res[1:2]), c("</>", "</>"))
})

test_that("easy_replace honors a custom replacement token", {
  res <- easy_replace("age", replace = "[X]")

  expect_named(res, c("<p>.*(age).*</p>", "(\n*[X])+\n*"))
  expect_equal(unname(res[["<p>.*(age).*</p>"]]), "[X]")
})

test_that("easy_recode splits named dots into parallel name and label vectors", {
  res <- easy_recode(c(a = "Label A"), c(b = "Label B", c = "Label C"))

  expect_named(res, c("name", "label"))
  expect_equal(res$name, c("a", "b", "c"))
  expect_equal(res$label, c("Label A", "Label B", "Label C"))
})

test_that("label_p returns a formatter rendering proportions as percentages", {
  withr::local_options(OutDec = ".")
  fmt <- label_p()

  expect_type(fmt, "closure")
  expect_equal(fmt(0.123), "12.3%")
  expect_equal(fmt(c(0.5, 0.05)), c("50.0%", "5.0%"))
})

test_that("label_p decimal mark follows a comma OutDec option", {
  withr::local_options(OutDec = ",")

  expect_equal(label_p()(0.123), "12,3%")
})

test_that("read_png reads a PNG file into a raster grob", {
  skip_if_not_installed("png")

  dir <- withr::local_tempdir()
  png::writePNG(
    array(0.5, dim = c(4, 4, 3)),
    target = file.path(dir, "img.png")
  )

  res <- read_png("img", dir = dir)

  expect_s3_class(res, "rastergrob")
})

test_that("wb_add_custom returns a workbook carrying the requested sheet", {
  skip_if_not_installed("openxlsx2")

  d <- data.frame(concept = c("A", "B"), text = c("x", "y"))

  res <- wb_add_custom(openxlsx2::wb_workbook(), sheet = "Custom", data = d)

  expect_s3_class(res, "wbWorkbook")
  expect_equal(unname(res$get_sheet_names()), "Custom")
})

test_that("wb_add_custom applies the concept and text font branches", {
  skip_if_not_installed("openxlsx2")

  d <- data.frame(concept = c("A", "B"), text = c("x", "y"))

  expect_no_error(
    wb_add_custom(
      openxlsx2::wb_workbook(),
      sheet = "Custom",
      data = d,
      concept_var = "concept",
      concept_color = "#FF0000",
      text_var = "text",
      text_color = "#00FF00"
    )
  )
})

test_that("logit_lty returns a tidied quantile model and a ggplot", {
  df <- data.frame(
    xv = seq_len(200),
    yv = factor(rep(c("no", "yes"), 100))
  )

  res <- logit_lty(df, yv, xv, breaks = 4)

  expect_s3_class(res$model, "tbl_df")
  expect_named(
    res$model,
    c("term", "estimate", "conf.low", "conf.high", "p.value")
  )
  expect_equal(nrow(res$model), 4)
  expect_s3_class(res$plot, "ggplot")
})
