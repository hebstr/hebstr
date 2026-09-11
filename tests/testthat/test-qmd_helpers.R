test_that("glue_qmd() interpolates with << >> delimiters", {
  x <- "world"
  expect_equal(glue_qmd("hello <<x>>"), glue::as_glue("hello world"))
})

test_that("glue_qmd() leaves curly braces untouched", {
  result <- glue_qmd("```{r}")
  expect_equal(result, glue::as_glue("```{r}"))
})

test_that("glue_qmd() evaluates in the caller's environment", {
  wrapper <- \() {
    val <- 42
    glue_qmd("result: <<val>>")
  }
  expect_equal(wrapper(), glue::as_glue("result: 42"))
})

test_that("glue_qmd() reports the class and length of a non-character input", {
  expect_error(glue_qmd(123), "<numeric> of length 1", class = "rlang_error")
  expect_error(glue_qmd(NULL), "<NULL> of length 0", class = "rlang_error")
  expect_error(glue_qmd(TRUE), "<logical> of length 1", class = "rlang_error")
})

test_that("glue_qmd() reports the length of a multi-element character vector", {
  expect_error(
    glue_qmd(c("a", "b")),
    "<character> of length 2",
    class = "rlang_error"
  )
})

test_that("tbl_qmd() returns a gt object from a data.frame", {
  result <- tbl_qmd(head(mtcars, 3))
  expect_s3_class(result, "gt_tbl")
})

test_that("tbl_qmd() respects top_n argument", {
  result <- tbl_qmd(mtcars, top_n = 2)
  expect_s3_class(result, "gt_tbl")
  expect_true(nrow(result[["_data"]]) < nrow(mtcars))
})

test_that("tbl_qmd() sets the html id on the data.frame and gtsummary paths", {
  .id_of <- \(x) {
    opts <- x[["_options"]]
    opts$value[opts$parameter == "table_id"][[1]]
  }

  tbl <- suppressMessages(gtsummary::tbl_summary(
    head(mtcars, 4),
    include = mpg
  ))

  expect_equal(.id_of(tbl_qmd(head(mtcars, 3), id = "tbl-a")), "tbl-a")
  expect_equal(.id_of(tbl_qmd(tbl, id = "tbl-b")), "tbl-b")
})

test_that("tbl_qmd() gives each table its own id by default", {
  .rendered_id <- \(x) {
    html <- gt::as_raw_html(x, inline_css = FALSE)
    match <- regmatches(html, regexpr('id="[^"]+"', html))
    sub('id="([^"]+)"', "\\1", match)
  }

  first <- .rendered_id(tbl_qmd(head(mtcars, 2)))
  second <- .rendered_id(tbl_qmd(head(mtcars, 2)))

  expect_length(first, 1)
  expect_false(identical(first, second))
})

test_that("tbl_qmd() leaves the id unset on the top_n path", {
  result <- tbl_qmd(mtcars, top_n = 2, id = "tbl-c")
  opts <- result[["_options"]]

  expect_true(is.na(opts$value[opts$parameter == "table_id"][[1]]))
})

test_that("tbl_qmd() applies custom font and size", {
  result <- tbl_qmd(head(iris, 2), font_family = "Arial", font_size = 20)
  opts <- result[["_options"]]
  font <- opts[opts$parameter == "table_font_names", "value"][[1]][[1]]
  size <- opts[opts$parameter == "table_font_size", "value"][[1]][[1]]
  expect_equal(font, "Arial")
  expect_equal(size, "20px")
})

test_that("tbl_qmd() default font_family reads the centralised text font", {
  local_hebstr("opts", list(font = list(alpha = "PinnedAlpha")))

  result <- tbl_qmd(head(mtcars, 3))
  opts_tbl <- result[["_options"]]
  font <- opts_tbl[opts_tbl$parameter == "table_font_names", "value"][[1]][[1]]

  expect_identical(font, "PinnedAlpha")
})

test_that("tbl_qmd() reports the class of an invalid data input", {
  expect_error(
    tbl_qmd("not a df"),
    "Object of class <character> supplied",
    class = "rlang_error"
  )
  expect_error(
    tbl_qmd(42),
    "Object of class <numeric> supplied",
    class = "rlang_error"
  )
  expect_error(
    tbl_qmd(list(a = 1)),
    "Object of class <list> supplied",
    class = "rlang_error"
  )
})

test_that("tbl_qmd() names top_n as the offending argument", {
  expect_error(
    tbl_qmd(mtcars, top_n = -1),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
  expect_error(
    tbl_qmd(mtcars, top_n = "a"),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
  expect_error(
    tbl_qmd(mtcars, top_n = c(1, 2)),
    "`top_n` must be a single positive numeric",
    class = "rlang_error"
  )
})

test_that("tbl_qmd() works with gtsummary objects", {
  tbl_sum <- gtsummary::trial[1:10, ] |> gtsummary::tbl_summary(include = age)
  result <- tbl_qmd(tbl_sum)
  expect_s3_class(result, "gt_tbl")
})

test_that("tbl_qmd(width = ) sizes the gt table in pixels", {
  res <- tbl_qmd(head(mtcars, 3), width = 500)

  expect_equal(
    res[["_options"]]$value[[which(
      res[["_options"]]$parameter == "table_width"
    )]],
    "500px"
  )
})

test_that("tbl_qmd(width = NULL) leaves the gt table at its natural width", {
  res <- tbl_qmd(head(mtcars, 3), width = NULL)

  expect_equal(
    res[["_options"]]$value[[which(
      res[["_options"]]$parameter == "table_width"
    )]],
    "auto"
  )
})

test_that("tbl_qmd(width = ) rejects a non-positive or non-scalar width", {
  expect_error(
    tbl_qmd(head(mtcars, 3), width = -1),
    "must be a single positive number"
  )
  expect_error(
    tbl_qmd(head(mtcars, 3), width = c(1, 2)),
    "must be a single positive number"
  )
  expect_error(
    tbl_qmd(head(mtcars, 3), page_width = 0),
    "must be a single positive number"
  )
})

test_that("tbl_qmd() routes a data frame to a themed flextable under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3))

  expect_s3_class(res, "flextable")
  expect_equal(res$body$dataset$mpg, head(mtcars, 3)$mpg)
})

test_that("tbl_qmd() routes a gtsummary through as_flex_table under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  tbl <- suppressMessages(gtsummary::tbl_summary(
    head(mtcars, 4),
    include = mpg
  ))

  res <- tbl_qmd(tbl)

  expect_s3_class(res, "flextable")
  expect_contains(res$body$dataset$label, "mpg")
})

test_that("tbl_qmd() renders the <br> of gtsummary headers as a line break under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  labels <- .ft_txt(tbl_qmd(gtsum_format(.make_summary_tbl())), part = "header")

  expect_false(any(stringr::str_detect(labels, stringr::fixed("<br>"))))
  expect_true(any(stringr::str_detect(labels, stringr::fixed("\n"))))
})

test_that("tbl_qmd() leaves the gt branch untouched when hebstr.docx is unset", {
  local_opts()
  withr::local_options(hebstr.docx = NULL)

  expect_s3_class(tbl_qmd(head(mtcars, 3)), "gt_tbl")
})

test_that("tbl_qmd(width = ) converts pixels to a page fraction under docx", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3), width = 500)

  expect_equal(res$properties$width, 500 / (6.5 * 96))
  expect_equal(res$properties$layout, "autofit")
})

test_that("tbl_qmd() caps the docx page fraction at 1 under its default width", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  expect_equal(tbl_qmd(head(mtcars, 3))$properties$width, 1)
})

test_that("tbl_qmd(page_width = ) sets the reference page width in inches", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3), width = 500, page_width = 8)

  expect_equal(res$properties$width, 500 / (8 * 96))
})

test_that("tbl_qmd(width = NULL) leaves the flextable at its natural width", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3), width = NULL)

  expect_equal(res$properties$width, 0)
  expect_equal(res$properties$layout, "fixed")
})

test_that("tbl_qmd() applies the font family and size on the flextable branch", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3), font_family = "Arial", font_size = 20)

  expect_true(all(res$body$styles$text$font.family$data == "Arial"))
  expect_true(all(res$body$styles$text$font.size$data == 20 * 0.75))
})

test_that("tbl_qmd() bolds the column labels on the flextable branch", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(head(mtcars, 3))

  expect_true(all(res$header$styles$text$bold$data))
})

test_that("tbl_qmd(top_n = ) previews the same rows on both branches", {
  local_opts()
  withr::local_options(hebstr.docx = TRUE)

  res <- tbl_qmd(mtcars, top_n = 2)

  expect_equal(nrow(res$body$dataset), 4)
  expect_contains(res$body$dataset$rowname, "3..31")
  expect_equal(.ft_txt(res, part = "header"), paste(names(mtcars), collapse = ""))
})

test_that("gt_qmd() warns of its deprecation and delegates to tbl_qmd()", {
  withr::local_options(lifecycle_verbosity = "warning")

  expect_snapshot(res <- gt_qmd(head(mtcars, 3)))
  expect_s3_class(res, "gt_tbl")
})

test_that("gt_qmd() forwards its arguments to tbl_qmd()", {
  res <- withr::with_options(
    list(lifecycle_verbosity = "quiet"),
    gt_qmd(head(mtcars, 3), font_family = "Arial")
  )

  opts <- res[["_options"]]

  expect_equal(opts[opts$parameter == "table_font_names", "value"][[1]][[1]], "Arial")
})

test_that("include_code_file() returns a glue string with correct structure", {
  result <- include_code_file("script.R")
  expect_s3_class(result, "glue")
  expect_match(result, "include='script.R'")
  expect_match(result, "code-line-numbers='true'")
  expect_match(result, "\\.r")
})

test_that("include_code_file() uses custom lang", {
  result <- include_code_file("query.sql", lang = "sql")
  expect_match(result, "\\.sql")
  expect_match(result, "include='query.sql'")
})

test_that("include_code_file() names src as the offending argument", {
  expect_error(include_code_file(123), "`src` must be", class = "rlang_error")
  expect_error(include_code_file(NULL), "`src` must be", class = "rlang_error")
  expect_error(
    include_code_file(c("a", "b")),
    "`src` must be",
    class = "rlang_error"
  )
})

test_that("include_code_file() names lang, not src, when only lang is invalid", {
  expect_error(
    include_code_file("script.R", lang = 1),
    "`lang` must be",
    class = "rlang_error"
  )
  expect_error(
    include_code_file("script.R", lang = NULL),
    "`lang` must be",
    class = "rlang_error"
  )
})

test_that("out_qmd() derives the artefact of a named object", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_demo <- grid::rectGrob()
  .make_out_files("output/fig-demo/fig-demo.png")

  expect_equal(as.character(out_qmd(fig_demo)), "output/fig-demo/fig-demo.png")
})

test_that("out_qmd() derives the artefact of one element of a list", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_codes <- list(cim10 = grid::rectGrob(), ccam = grid::circleGrob())
  .make_out_files("output/fig-codes/fig-codes-cim10.png")

  expect_equal(
    as.character(out_qmd(fig_codes$cim10)),
    "output/fig-codes/fig-codes-cim10.png"
  )
})

test_that("out_qmd() publishes a bare list as one multi-panel figure", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_voies <- list(brut = grid::rectGrob(), branche = grid::circleGrob())
  .make_out_files(
    "output/fig-voies/fig-voies-brut.png",
    "output/fig-voies/fig-voies-branche.png"
  )

  expect_equal(
    as.character(out_qmd(fig_voies)),
    c(
      "output/fig-voies/fig-voies-brut.png",
      "output/fig-voies/fig-voies-branche.png"
    )
  )
})

test_that("out_qmd() suffixes the file and leaves the folder on the base", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_demo <- grid::rectGrob()
  .make_out_files("output/fig-demo/fig-demo-v2.png")

  expect_equal(
    as.character(out_qmd(fig_demo, suffix = "v2")),
    "output/fig-demo/fig-demo-v2.png"
  )
})

test_that("out_qmd() honours subdir", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_demo <- grid::rectGrob()
  .make_out_files("output/fig-demo.png", "output/figs/fig-demo.png")

  expect_equal(
    as.character(out_qmd(fig_demo, subdir = FALSE)),
    "output/fig-demo.png"
  )
  expect_equal(
    as.character(out_qmd(fig_demo, subdir = "figs")),
    "output/figs/fig-demo.png"
  )
})

test_that("out_qmd() honours filename and dir", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_demo <- grid::rectGrob()
  .make_out_files(
    "output/fig-custom/fig-custom.png",
    "out/fig-demo/fig-demo.png"
  )

  expect_equal(
    as.character(out_qmd(fig_demo, filename = "fig_custom")),
    "output/fig-custom/fig-custom.png"
  )
  expect_equal(
    as.character(out_qmd(fig_demo, dir = "out")),
    "out/fig-demo/fig-demo.png"
  )
})

test_that("out_qmd() reads the directory option easy_out() writes to", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")
  withr::local_options(easy_out.dir = "artefacts")

  fig_demo <- grid::rectGrob()
  .make_out_files("artefacts/fig-demo/fig-demo.png")

  expect_equal(
    as.character(out_qmd(fig_demo)),
    "artefacts/fig-demo/fig-demo.png"
  )
})

test_that("out_qmd() keeps a numeric token attached to its word", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_cim10 <- grid::rectGrob()
  .make_out_files("output/fig-cim10/fig-cim10.png")

  expect_equal(
    as.character(out_qmd(fig_cim10)),
    "output/fig-cim10/fig-cim10.png"
  )
})

test_that("out_qmd() reads back the file easy_out() wrote", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  fig_demo <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()

  local_mocked_bindings(
    ggsave = \(filename, ...) writeLines("<svg ></svg>", filename),
    image_read_svg = \(...) "mock_img",
    image_write = \(image, path, ...) {
      fs::file_create(path)
      invisible(NULL)
    },
    browseURL = \(...) invisible(NULL)
  )

  easy_out(fig_demo, quiet = TRUE)

  expect_equal(as.character(out_qmd(fig_demo)), "output/fig-demo/fig-demo.png")
})

test_that("out_qmd() hands a gt table over live under HTML", {
  skip_if_not_installed("knitr")

  local_pandoc_to("html")

  tbl_demo <- .make_gt_mtcars()

  expect_identical(out_qmd(tbl_demo), tbl_demo)
})

test_that("out_qmd() hands a gt table over as a PNG under Word", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  tbl_demo <- .make_gt_mtcars()
  .make_out_files("output/tbl-demo/tbl-demo.png")

  expect_equal(as.character(out_qmd(tbl_demo)), "output/tbl-demo/tbl-demo.png")
})

test_that("out_qmd() hands a gtsummary table over live under HTML", {
  skip_if_not_installed("knitr")

  local_pandoc_to("html")

  tbl_demo <- .make_summary_tbl()

  expect_identical(out_qmd(tbl_demo), tbl_demo)
})

test_that("out_qmd() hands a flextable over live whichever the target", {
  skip_if_not_installed("knitr")

  ft_demo <- .make_ft_mtcars()

  local_pandoc_to("html")
  expect_identical(out_qmd(ft_demo), ft_demo)

  local_pandoc_to("docx")
  expect_identical(out_qmd(ft_demo), ft_demo)
})

test_that("out_qmd() hands a widget over live under HTML", {
  skip_if_not_installed("knitr")

  local_pandoc_to("html")

  wdg_demo <- reactable::reactable(head(mtcars))

  expect_identical(out_qmd(wdg_demo), wdg_demo)
})

test_that("out_qmd() hands a widget over as a PNG under Word", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  wdg_demo <- reactable::reactable(head(mtcars))
  .make_out_files("output/wdg-demo/wdg-demo.png")

  expect_equal(as.character(out_qmd(wdg_demo)), "output/wdg-demo/wdg-demo.png")
})

test_that("out_qmd() unwraps a dictionary to its widget under HTML", {
  skip_if_not_installed("knitr")

  local_pandoc_to("html")

  dict_demo <- .make_dict()

  expect_identical(out_qmd(dict_demo), dict_demo$output)
})

test_that("out_qmd() hands a dictionary over as a PNG under Word", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("docx")

  dict_demo <- .make_dict()
  .make_out_files("output/dict-demo/dict-demo.png")

  expect_equal(
    as.character(out_qmd(dict_demo)),
    "output/dict-demo/dict-demo.png"
  )
})

test_that("out_qmd() hands a figure over as its SVG under HTML", {
  skip_if_not_installed("knitr")

  withr::local_dir(withr::local_tempdir())
  local_pandoc_to("html")

  fig_demo <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) +
    ggplot2::geom_point()
  .make_out_files("output/fig-demo/fig-demo.svg")

  expect_equal(as.character(out_qmd(fig_demo)), "output/fig-demo/fig-demo.svg")
})

test_that("out_qmd() names a workbook apart from an unsupported class", {
  skip_if_not_installed("knitr")

  wb_demo <- get_xlsx(list(iris = head(datasets::iris)))

  expect_error(out_qmd(wb_demo), "is a workbook", class = "rlang_error")
})

test_that("out_qmd() reports the class of an unsupported object", {
  skip_if_not_installed("knitr")

  df_demo <- head(mtcars)

  expect_error(out_qmd(df_demo), "<data.frame>", class = "rlang_error")
})

test_that("out_qmd() refuses an expression that is not a name", {
  skip_if_not_installed("knitr")

  fig_demo <- grid::rectGrob()
  fig_list <- list(brut = grid::rectGrob())

  expect_error(
    out_qmd(identity(fig_demo)),
    "named object",
    class = "rlang_error"
  )
  expect_error(
    out_qmd(fig_list[["brut"]]),
    "named object",
    class = "rlang_error"
  )
  expect_error(
    out_qmd(fig_list$brut$children),
    "named object",
    class = "rlang_error"
  )
})

test_that("out_qmd() refuses an unnamed expression even under a filename", {
  skip_if_not_installed("knitr")

  fig_demo <- grid::rectGrob()

  expect_error(
    out_qmd(identity(fig_demo), filename = "fig_custom"),
    "named object",
    class = "rlang_error"
  )
})

test_that("out_qmd() refuses an empty or unnamed list", {
  skip_if_not_installed("knitr")

  fig_empty <- list()
  fig_unnamed <- list(grid::rectGrob())

  expect_error(out_qmd(fig_empty), "named, non-empty", class = "rlang_error")
  expect_error(out_qmd(fig_unnamed), "named, non-empty", class = "rlang_error")
})

test_that("out_qmd() refuses a list holding anything but figures", {
  skip_if_not_installed("knitr")

  fig_mixed <- list(brut = grid::rectGrob(), tbl = .make_gt_mtcars())

  expect_error(out_qmd(fig_mixed), "figures only", class = "rlang_error")
  expect_error(out_qmd(fig_mixed), "gt_tbl", class = "rlang_error")
})

test_that("out_qmd() guards its path-forming arguments", {
  skip_if_not_installed("knitr")

  fig_demo <- grid::rectGrob()

  expect_error(
    out_qmd(fig_demo, filename = c("a", "b")),
    "`filename` must be",
    class = "rlang_error"
  )
  expect_error(
    out_qmd(fig_demo, subdir = 1),
    "`subdir` must be",
    class = "rlang_error"
  )
  expect_error(
    out_qmd(fig_demo, suffix = NA),
    "`suffix` must be",
    class = "rlang_error"
  )
  expect_error(
    out_qmd(fig_demo, sep = c("_", "-")),
    "`sep` must be",
    class = "rlang_error"
  )
})
