test_that("get_vars_dict() propagates the centralised text font into the widget", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  view <- get_vars_dict(head(mtcars), strip_color = "#fff")

  expect_true(any(grepl("PinnedAlpha", unlist(view), fixed = TRUE)))
})

test_that("get_vars_dict() renders standalone when opts is absent", {
  local_hebstr("opts")

  expect_no_error(get_vars_dict(head(mtcars)))
})

test_that("get_vars_dict() assembles the descriptive data columns", {
  df <- tibble::tibble(
    bin_num = c(1, 1, 2, 2, 2),
    cont = c(10, 20, 30, 40, NA),
    grp = c("x", "y", "x", "z", "y")
  )

  d <- get_vars_dict(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "bin_num"]), "bin")
  expect_equal(unname(d$type[d$variable == "cont"]), "num")

  expect_true(is.na(d$n_miss[d$variable == "bin_num"]))
  expect_equal(unname(d$n_miss[d$variable == "cont"]), 1)
  expect_equal(unname(d$p_miss[d$variable == "cont"]), "20.0%")

  expect_equal(unname(d$range[d$variable == "cont"]), "10 ; 40")
  expect_equal(
    unname(d$q1_med_q3[d$variable == "cont"]),
    "17.5 ; 25 ; 32.5"
  )
  expect_true(is.na(d$q1_med_q3[d$variable == "bin_num"]))
})

test_that("get_vars_dict() reports the type and levels of categorical columns", {
  df <- tibble::tibble(
    grp = factor(c("x", "y", "x", "z", "y")),
    chr = c("a", "bb", "a", "a", "bb"),
    visit = as.Date("2020-01-01") + c(0, 10, 5, 30, 2)
  )

  d <- get_vars_dict(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "grp"]), "fct")
  expect_equal(unname(d$levels[d$variable == "grp"]), "x ; y ; z")
  expect_equal(unname(d$type[d$variable == "chr"]), "chr")
})

test_that("get_vars_dict() restricts the widget to the selected columns", {
  view <- get_vars_dict(
    head(mtcars),
    cols = c(variable, type),
    strip_color = "#fff"
  )

  expect_equal(names(.view_cols(view)), c("variable", "type"))
  expect_true(all(c("pos", "range") %in% names(view$data)))
})

test_that("get_vars_dict() shows every summary column by default", {
  view <- get_vars_dict(head(mtcars), strip_color = "#fff")

  expect_equal(
    names(.view_cols(view)),
    c(
      "n",
      "variable",
      "label",
      "type",
      "n_miss",
      "p_miss",
      "range",
      "q1_med_q3"
    )
  )
})

test_that("get_vars_dict() selects on the displayed name of the position column", {
  view <- get_vars_dict(
    head(mtcars),
    cols = c(n, variable),
    strip_color = "#fff"
  )

  expect_equal(names(.view_cols(view)), c("n", "variable"))
  expect_named(view$data[1], "pos")
})

test_that("get_vars_dict() aborts when the selection is empty", {
  expect_error(
    get_vars_dict(head(mtcars), cols = starts_with("zz"), strip_color = "#fff"),
    "must select at least one column"
  )
})

test_that("get_vars_dict() reports the range of Date columns", {
  df <- tibble::tibble(
    grp = factor(c("x", "y", "x", "z", "y")),
    visit = as.Date("2020-01-01") + c(0, 10, 5, 30, 2)
  )

  d <- get_vars_dict(df, strip_color = "#fff")$data

  expect_equal(unname(d$type[d$variable == "visit"]), "date")
  expect_equal(
    unname(d$range[d$variable == "visit"]),
    "2020-01-01 ; 2020-01-31"
  )
})

test_that("get_vars_dict() sizes the widget columns on their content", {
  short <- get_vars_dict(tibble::tibble(v = 1:3), strip_color = "#fff")

  wide <- tibble::tibble(v = 1:3) |>
    setNames("a_considerably_longer_variable_name") |>
    get_vars_dict(strip_color = "#fff")

  expect_gt(.view_cols(wide)[["variable"]], .view_cols(short)[["variable"]])
})

test_that("get_vars_dict() keeps the widget widths within their bounds", {
  df <- tibble::tibble(v = 1:3)
  labelled::var_label(df$v) <- strrep("long ", 100)

  widths <- .view_cols(get_vars_dict(df, strip_color = "#fff"))

  expect_equal(widths[["label"]], 300)
  expect_equal(widths[["n"]], 50)
})

test_that("get_vars_dict() scales the widget widths with the font size", {
  small <- .view_cols(
    get_vars_dict(head(mtcars), font_size = "0.7rem", strip_color = "#fff")
  )
  large <- .view_cols(
    get_vars_dict(head(mtcars), font_size = "1.4rem", strip_color = "#fff")
  )

  expect_gt(large[["variable"]], small[["variable"]])
})

test_that("get_vars_dict(json = TRUE) keeps the multi-valued cells structured", {
  df <- tibble::tibble(cont = c(10, 20, 30, 40, NA))

  view <- get_vars_dict(df, json = TRUE, strip_color = "#fff")

  expect_null(get_vars_dict(df, strip_color = "#fff")$json)
  expect_type(view$json$range, "list")
  expect_equal(as.numeric(view$json$range[[1]]), c(10, 40))
  expect_equal(unname(view$json$n_miss), 1)
  expect_equal(unname(view$json$p_miss), 0.2)
})
