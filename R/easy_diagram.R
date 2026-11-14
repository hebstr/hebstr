#' Declare a Mermaid diagram for export
#'
#' Wraps the text of a Mermaid diagram in an object [easy_out()] writes as a
#' figure and [out_qmd()] publishes, the way a ggplot goes through the same two
#' verbs. Nothing is rendered here: the text is only checked and kept, so a
#' script builds the object on a machine without Node.js, and the render waits
#' for the export.
#'
#' @param text The diagram, as a single string of Mermaid text, for example an
#'   `erDiagram`.
#'
#' @details
#' The export renders the text with [Pele](https://github.com/obsidianmd/pele),
#' which the package ships, run through Node.js 20 or later. Node is required
#' only by [easy_out()] when it writes the diagram, never to load the package or
#' to build the object, and the call aborts with a message if it is missing.
#'
#' @returns A `hebstr_diagram` object: a list holding `text`.
#' @export
#'
#' @examples
#' d <- easy_diagram("erDiagram\n  ORDER }o--|| CUSTOMER : \"\"")
#'
#' \dontrun{
#' easy_out(d)
#' }
#'
easy_diagram <- \(text) {
  if (!is_scalar_character(text) || is.na(text) || !nzchar(str_trim(text))) {
    cli_abort(c(
      "{.arg text} must be a single non-blank string of Mermaid text.",
      "x" = "Received object of class: {.cls {class(text)}}"
    ))
  }

  structure(list(text = text), class = c("hebstr_diagram", "list"))
}

.diagram_min_node <- 20L

.diagram_node <- \() {
  node <- unname(Sys.which("node"))
  need <- .diagram_min_node

  if (!nzchar(node)) {
    cli_abort(c(
      "Rendering a diagram needs Node.js, which is not on the {.envvar PATH}.",
      "i" = "Install Node.js {.val {need}} or later, then restart R."
    ))
  }

  version <- suppressWarnings(system2(node, "--version", stdout = TRUE, stderr = FALSE))
  major <- suppressWarnings(as.integer(str_extract(version[1], "^v(\\d+)", group = 1)))

  if (is.na(major) || major < .diagram_min_node) {
    cli_abort(c(
      "Rendering a diagram needs Node.js {.val {need}} or later.",
      "x" = "Found {.val {version[1]}} at {.path {node}}."
    ))
  }

  node
}

.diagram_render <- \(text, family = NULL) {
  node <- .diagram_node()

  family <- family %||% .text_font()
  runner <- system.file("pele", "render.mjs", package = "hebstr")

  input <- withr::local_tempfile(fileext = ".mmd")
  errors <- withr::local_tempfile(fileext = ".txt")

  writeLines(text, input, useBytes = TRUE)

  out <- suppressWarnings(system2(
    node,
    c(shQuote(runner), shQuote(input), shQuote(.font_css(family))),
    stdout = TRUE,
    stderr = errors
  ))

  status <- attr(out, "status") %||% 0L

  if (status != 0L) {
    cli_abort(c(
      "Pele could not render the diagram.",
      "x" = "{str_squish(readLines(errors, warn = FALSE, encoding = 'UTF-8'))}"
    ))
  }

  Encoding(out) <- "UTF-8"

  fromJSON(paste(out, collapse = ""))
}

.diagram_ink <- "#333333"

.diagram_theme <- \() {
  color <- set_opts(.assign = FALSE)$color

  c(
    bg = "#ffffff",
    fg = .diagram_ink,
    line = .diagram_ink,
    surface = color$cold[1],
    border = color$base,
    accent = color$cold[2]
  )
}

.diagram_svg <- \(text, family = NULL) {
  family <- family %||% .text_font()
  out <- .diagram_render(text, family)

  theme <- .diagram_theme()
  vars <- str_c("--pele-", names(theme), ":", theme, ";", collapse = "")
  faces <- .css_faces(family) %||% ""
  style <- str_c("<style>", faces, ".pele{", vars, "}</style>")

  root_end <- str_locate(out$svg, ">")[1, "end"]
  out$svg <- str_c(
    str_sub(out$svg, 1, root_end),
    style,
    str_sub(out$svg, root_end + 1)
  )

  out
}

.diagram_zoom <- 2

.diagram_png <- \(to_svg, to_png, width, height) {
  page <- fs::file_temp(tmp_dir = fs::path_dir(to_svg), pattern = "diagram-", ext = "html")
  on.exit(unlink(page), add = TRUE)

  writeLines(
    str_c(
      '<!DOCTYPE html><meta charset="utf-8"><body style="margin:0">',
      '<img src="',
      fs::path_file(to_svg),
      '" style="display:block">'
    ),
    page
  )

  webshot(
    url = page,
    file = to_png,
    vwidth = width,
    vheight = height,
    selector = "img",
    zoom = .diagram_zoom,
    quiet = TRUE
  )

  invisible(to_png)
}
