### DATA -----------------------------------------------------------------------

.make_summary_df <- \() {
  data.frame(
    grp = rep(c("a", "b"), 10),
    age = c(1:10, 5:14),
    sex = factor(rep(c("m", "f"), 10))
  )
}

.tbl_data <- \() {
  data.frame(
    label = c("Alpha", "Beta", "Gamma"),
    stat_1 = c(1, 2, 3),
    stringsAsFactors = FALSE
  )
}

### GT / FLEXTABLE --------------------------------------------------------------

.make_gt_stat <- \() gt::gt(.tbl_data())

.make_ft_stat <- \() flextable::flextable(.tbl_data())

.make_gt_mtcars <- \() gt::gt(head(mtcars))

.make_ft_mtcars <- \() flextable::flextable(head(mtcars))

### GTSUMMARY -------------------------------------------------------------------

.make_summary_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = seq_len(20),
    sex = factor(c(rep("m", 10), rep(c("m", "f"), 5)))
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = c(age, sex))
  )
}

.make_missing_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = c(seq_len(18), NA, NA),
    sex = factor(c(rep("m", 9), NA, rep(c("m", "f"), 5)))
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = c(age, sex))
  )
}

.make_complete_tbl <- \() {
  df <- data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = seq_len(20)
  )

  suppressMessages(
    gtsummary::tbl_summary(df, by = grp, include = age)
  )
}

.make_note_tbl <- \() {
  gtsummary::tbl_summary(
    data.frame(
      age = c(30, 40, 50, 60),
      sex = factor(c("F", "M", "F", "M"))
    ),
    include = c(age, sex)
  )
}

.make_reg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15)
  )
  mod <- glm(y ~ x, data = df, family = binomial())

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
}

.make_uvreg_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    x = seq_len(30) + rep(c(0, 2), 15),
    z = seq_len(30) - rep(c(0, 1), 15)
  )
  gtsummary::tbl_uvregression(
    df,
    method = glm,
    y = y,
    method.args = list(family = binomial()),
    exponentiate = TRUE
  )
}

.make_no_event_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 15),
    g = factor(rep(c("a", "b", "c"), 10))
  )
  df$y[df$g == "c"] <- 0

  suppressWarnings(
    gtsummary::tbl_uvregression(
      df,
      method = glm,
      y = y,
      method.args = list(family = binomial()),
      exponentiate = TRUE
    )
  )
}

.make_ref_tbl <- \() {
  df <- data.frame(
    y = rep(c(0, 1), 30),
    grp = factor(
      rep(c("low", "mid", "high"), 20),
      levels = c("low", "mid", "high")
    )
  )
  mod <- glm(y ~ grp, data = df, family = binomial())
  gtsummary::tbl_regression(mod, exponentiate = TRUE)
}

### EXTRACTORS ------------------------------------------------------------------

.ft_txt <- \(x, part = "footer") {
  d <- x[[part]]$content$data

  vapply(
    seq_len(nrow(d)),
    \(i) {
      paste0(
        vapply(
          seq_len(ncol(d)),
          \(j) {
            ch <- d[[i, j]]
            if (is.null(ch) || !nrow(ch)) "" else paste(ch$txt, collapse = "")
          },
          character(1)
        ),
        collapse = ""
      )
    },
    character(1)
  )
}

.styles_for <- \(x, col) {
  .keep <- !is.na(x$`_styles`$colname) & x$`_styles`$colname == col

  x$`_styles`$styles[.keep]
}

.note_labels <- \(x) {
  rows <- x$table_styling$footnote_body$rows[[1]]

  x$table_body$label[eval_tidy(rows, data = x$table_body)]
}

.bold_fonts <- \(wb) {
  fonts <- wb$styles_mgr$styles$fonts
  fonts[grepl("<b val=\"1\"/>", fonts, fixed = TRUE)]
}
