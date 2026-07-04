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

test_that("str_u joins patterns into a regex alternation", {
  expect_equal(str_u("75", "77", "91"), "75|77|91")
  expect_equal(str_u("abc"), "abc")
})

test_that("str_color wraps text in a styled bold HTML span", {
  res <- as.character(str_color("hello", color = "blue"))

  expect_match(res, "color:blue;")
  expect_match(res, "background-color:#ffffff00;")
  expect_match(res, "\\*\\*hello\\*\\*")
})

test_that("str_fig uses an inline font-size span outside Quarto", {
  res <- as.character(str_fig("Title", note = "Note", acro = "AC"))

  expect_match(res, "font-size:7.5pt")
  expect_match(res, "Note AC")
  expect_no_match(res, "class=")
})

test_that("str_fig uses a CSS class span in Quarto mode", {
  res <- as.character(str_fig("Title", note = "Note", qmd = TRUE))

  expect_match(res, "class='quarto-float-subcaption'")
  expect_no_match(res, "font-size:")
})

test_that("str_label accepts bare unquoted variable names (data-masking)", {
  df <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  labelled::var_label(df$age) <- "Age"
  labelled::var_label(df$sex) <- "Sexe"

  expect_equal(str_label(df, age, sex), "Age and Sexe")
})

test_that("str_label still accepts quoted variable names", {
  df <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  labelled::var_label(df$age) <- "Age"
  labelled::var_label(df$sex) <- "Sexe"

  expect_equal(str_label(df, "age", "sex"), "Age and Sexe")
})

test_that("str_label tolerates an unlabelled variable instead of crashing", {
  df <- data.frame(age = 1:3, sex = c("F", "M", "F"))
  labelled::var_label(df$age) <- "Age"

  expect_equal(str_label(df, age, sex), "Age and ")
})
