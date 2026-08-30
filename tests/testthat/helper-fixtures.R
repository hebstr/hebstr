### DATA -----------------------------------------------------------------------

.make_summary_df <- \() {
  data.frame(
    grp = c(rep("a", 10), rep("b", 10)),
    age = c(1:10, 5:14),
    sex = factor(c(rep("m", 6), rep("f", 4), rep("m", 4), rep("f", 6)))
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

.make_gt_titled <- \() gt::tab_header(gt::gt(head(mtcars)), title = "T")

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

.make_pois_tbl <- \() {
  df <- data.frame(
    y = rep(c(1L, 2L, 3L, 4L, 5L, 6L), 20),
    x = factor(rep(c("a", "b"), each = 60))
  )
  mod <- glm(y ~ x, data = df, family = quasipoisson())

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
}

.make_rr_tbl <- \() {
  df <- data.frame(
    y = c(rep(c(1, 0, 0, 0, 0), 12), rep(c(1, 1, 0, 0, 0), 12)),
    x = factor(rep(c("a", "b"), each = 60))
  )
  mod <- glm(
    y ~ x,
    data = df,
    family = binomial(link = "log"),
    start = c(log(0.2), log(2))
  )

  list(tbl = gtsummary::tbl_regression(mod, exponentiate = TRUE), mod = mod)
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

.make_check_df <- \() {
  data.frame(
    rowname = 1:4,
    site = c("A", "B", "A", "B"),
    start = as.Date(c("2024-01-01", NA, "2024-03-01", "2024-04-01")),
    end = as.Date(c("2023-12-01", "2024-02-01", "2024-04-01", NA)),
    n_first = c(0, 3, 0, 2),
    n_second = c(1, 0, 0, 2)
  )
}

.make_dict <- \(...) {
  get_vars_dict(head(datasets::iris, 10), ...)
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

.gt_align <- \(x, locname) {
  styles <- x$`_styles`

  vapply(
    styles$styles[styles$locname == locname],
    \(style) style$cell_text$align %||% NA_character_,
    character(1)
  )
}

.gt_heading_align <- \(x) {
  opts <- x$`_options`

  opts$value[opts$parameter == "heading_align"][[1]]
}

.note_labels <- \(x) {
  rows <- x$table_styling$footnote_body$rows[[1]]

  x$table_body$label[eval_tidy(rows, data = x$table_body)]
}

.cell_xf <- \(wb, dims, sheet = 1L) {
  styles <- wb$styles_mgr$styles
  xfs <- vapply(
    openxlsx2::wb_get_cell_style(wb, sheet = sheet, dims = dims),
    \(id) if (nzchar(id)) styles$cellXfs[[as.integer(id) + 1L]] else "",
    character(1)
  )

  # wb_get_cell_style() answers in the workbook's own cell order, not in dims order
  xfs[dims]
}

.cell_style_ref <- \(wb, dims, sheet, id_attr, registry) {
  xfs <- .cell_xf(wb, dims, sheet)
  ids <- as.integer(gsub(paste0('.*', id_attr, '="(\\d+)".*'), "\\1", xfs))

  set_names(
    ifelse(is.na(ids), "", wb$styles_mgr$styles[[registry]][ids + 1L]),
    names(xfs)
  )
}

.cell_borders <- \(wb, dims, sheet = 1L) {
  .cell_style_ref(wb, dims, sheet, "borderId", "borders")
}

.cell_fonts <- \(wb, dims, sheet = 1L) {
  .cell_style_ref(wb, dims, sheet, "fontId", "fonts")
}

.cell_fills <- \(wb, dims, sheet = 1L) {
  .cell_style_ref(wb, dims, sheet, "fillId", "fills")
}

# alignment is an attribute of the cellXf itself, not a reference into a registry
.cell_halign <- \(wb, dims, sheet = 1L) {
  xfs <- .cell_xf(wb, dims, sheet)

  set_names(stringr::str_match(xfs, 'horizontal="([a-z]+)"')[, 2], names(xfs))
}

.sheet_halign <- \(wb, data, row, sheet = 1L) {
  dims <- vapply(
    seq_along(data),
    \(col) openxlsx2::wb_dims(rows = row, cols = col),
    character(1)
  )

  set_names(.cell_halign(wb, dims, sheet), names(data))
}

.view_cols <- \(x, key = "minWidth") {
  defs <- x$output$x$tag$attribs$columns

  setNames(
    sapply(defs, \(col) col[[key]]),
    sapply(defs, \(col) col$id)
  )
}

### SVG -------------------------------------------------------------------------

.make_svg <- \(grob, bg = "white", width = 9, height = 8) {
  path <- withr::local_tempfile(fileext = ".svg", .local_envir = parent.frame())

  svglite::svglite(path, width = width, height = height, bg = bg)
  grid::grid.newpage()

  if (!is.null(grob)) {
    grid::grid.draw(grob)
  }

  grDevices::dev.off()

  path
}

.local_device <- \(width, height, .local_envir = parent.frame()) {
  svglite::svgstring(width = width, height = height)
  device <- grDevices::dev.cur()

  withr::defer(
    if (device %in% grDevices::dev.list()) grDevices::dev.off(device),
    envir = .local_envir
  )

  device
}

.view_box <- \(path) {
  path |>
    readLines() |>
    grepv(pattern = "<svg[ >]") |>
    stringr::str_extract("viewBox\\s*=\\s*'([^']*)'", group = 1) |>
    strsplit(" ") |>
    unlist() |>
    as.numeric()
}

### PPTX -------------------------------------------------------------------------

# geometry of the DrawingML group, in inches (OOXML stores it in EMU)
# officer offers no reader for a written file, so the part is read as text
.docx_body <- \(path) {
  dir <- withr::local_tempdir()

  utils::unzip(path, files = "word/document.xml", exdir = dir)

  fs::path(dir, "word", "document.xml") |>
    readLines(warn = FALSE) |>
    paste(collapse = "")
}


.slide_frame <- \(path) {
  dir <- withr::local_tempdir()

  utils::unzip(path, files = "ppt/slides/slide1.xml", exdir = dir)

  frame <-
    fs::path(dir, "ppt", "slides", "slide1.xml") |>
    readLines(warn = FALSE) |>
    paste(collapse = "") |>
    stringr::str_extract("<p:grpSp .*?</a:xfrm>")

  emu <- \(pattern) {
    as.numeric(stringr::str_extract(frame, pattern, group = 1)) / 914400
  }

  c(
    left = emu('<a:off x="([0-9]+)"'),
    top = emu('<a:off x="[0-9]+" y="([0-9]+)"'),
    width = emu('<a:ext cx="([0-9]+)"'),
    height = emu('<a:ext cx="[0-9]+" cy="([0-9]+)"')
  )
}

### QUARTO EXTENSION -------------------------------------------------------------

.make_extension <- \(root, id = "org/ext", reference_doc = "template.dotx") {
  dir <- fs::path(root, "_extensions", id)
  fs::dir_create(dir)

  if (!is.null(reference_doc)) {
    # officer refuses to write any extension but .docx; a reference-doc is a .dotx
    written <- fs::file_temp(ext = "docx")

    officer::read_docx() |>
      officer::body_set_default_section(
        officer::prop_section(
          page_size = officer::page_size(width = 8.5, height = 11),
          page_margins = officer::page_mar(left = 1, right = 1)
        )
      ) |>
      print(target = written)

    fs::file_move(written, fs::path(dir, reference_doc))
  }

  list(
    contributes = list(
      formats = list(
        docx = c(
          list(`number-sections` = FALSE),
          if (!is.null(reference_doc)) list(`reference-doc` = reference_doc)
        )
      )
    )
  ) |>
    yaml::write_yaml(fs::path(dir, "_extension.yml"))

  dir
}
