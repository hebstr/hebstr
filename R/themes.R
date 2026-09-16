#' Check whether font families are installed
#'
#' Tests one or more font families against the system font list (via
#' [systemfonts::system_fonts()]) and the session registry (via
#' [systemfonts::registry_fonts()]), so a family registered from bundled font
#' files counts as available. Family names match in full, ignoring case: a
#' name shared with a wider family (`"Liberation"` against `"Liberation Sans"`)
#' counts as missing, since rendering it falls back to the system default. The
#' device aliases `"sans"`, `"serif"` and `"mono"` always count as available.
#'
#' @param ... One or more font family names to test for installation.
#' @param .default Font family to fall back to when `.auto` is not installed.
#' @param .auto A single font family to resolve: returns it when installed,
#'   otherwise `.default`. When supplied, `...` must be empty.
#' @param .abort Whether to raise an error listing the missing families instead
#'   of returning `FALSE`.
#'
#' @return With `.auto`, the resolved font family (the requested one or
#'   `.default`). Otherwise `TRUE` when every family in `...` is installed,
#'   `FALSE` when at least one is missing (unless `.abort` is `TRUE`).
#' @export
#'
#' @examples
#' check_fonts("sans")
#'
#' check_fonts(.auto = "luciole")
#'
check_fonts <- \(..., .default = "sans", .auto = NULL, .abort = FALSE) {
  if (!is.null(.auto)) {
    check_dots_empty()

    if (!check_fonts(.auto)) .default else .auto
  } else {
    fonts <- unlist(list(...))

    system <- c(
      unique(systemfonts::system_fonts()$family),
      unique(systemfonts::registry_fonts()$family),
      c("sans", "serif", "mono")
    )

    is_installed <- tolower(fonts) %in% tolower(system)

    if (FALSE %in% is_installed) {
      which_font <-
        data.frame(fonts, is_installed) |>
        filter(!is_installed) |>
        pull(fonts)

      if (.abort) {
        cli::cli_abort("{which_font} font{?s} {?is/are} not installed")
      }

      return(FALSE)
    }

    return(TRUE)
  }
}


.text_font <- \() {
  if (exists("opts", envir = .hebstr, inherits = FALSE)) {
    check_opts(font$alpha)
  } else {
    "sans"
  }
}


# Word's default since 2024 first, then the family it replaced: a reader on
# either generation lands on an installed face rather than on the renderer's
# own guess. Read by the CSS stacks below and by the w:altName of the docx
# branch, which is the same intent expressed twice.
.font_fallback <- c("Aptos", "Calibri")


# CSS takes an ordered stack where a run of OOXML takes a single name. A bare
# family is what leaves a Windows reader on Times New Roman, the package
# default included: "sans" is a device generic of R, never a CSS one, and
# nothing outside fontconfig resolves it.
.font_stack <- \(family) c(family, .font_fallback, "sans-serif")


# gt quotes the stack it is handed; raw CSS has to be quoted here, or a family
# whose name carries a space reads as several keywords
.font_css <- \(family) {
  stack <- .font_stack(family)

  ifelse(str_detect(stack, " "), paste0("'", stack, "'"), stack) |>
    paste(collapse = ", ")
}


# Faces the package ships, keyed by family in lowercase: a family reaches here
# as the user spelled it in set_opts().
.bundled_faces <- list(
  luciole = list(
    c(file = "Luciole-Regular.woff2", weight = "400"),
    c(file = "Luciole-Bold.woff2", weight = "700")
  )
)


# OOXML embeds a face as a TrueType or OpenType part, so the woff2 above cannot
# serve it, and Word picks the italic slots from the font table where a web page
# would synthesise them. The two inventories therefore hold the same family in
# two formats, keyed alike.
.docx_faces <- list(
  luciole = c(
    regular = "Luciole-Regular.ttf",
    bold = "Luciole-Bold.ttf",
    italic = "Luciole-Regular-Italic.ttf",
    bold_italic = "Luciole-Bold-Italic.ttf"
  )
)


# Paths of the faces to embed, by docx_embed_font() slot. The shipped files come
# first: they are the same version everywhere, where a system install is
# whatever the writing machine happens to carry. A generic alias and a family
# installed in a format OOXML cannot hold both return NULL, leaving the fallback
# names as the only thing the document can say about them.
.font_faces <- \(family) {
  bundled <- .docx_faces[[tolower(family)]]

  if (!is.null(bundled)) {
    # file.path() drops the names, and the slots are what the caller splices
    return(as.list(set_names(
      file.path(system.file("fonts", package = "hebstr"), bundled),
      names(bundled)
    )))
  }

  # a family often carries condensed faces under the same name, which would
  # otherwise fill a slot the regular one is meant to hold
  fonts <- systemfonts::system_fonts()
  fonts <- fonts[
    tolower(fonts$family) == tolower(family) &
      fonts$width %in% "normal" &
      tolower(fs::path_ext(fonts$path)) %in% c("ttf", "otf"),
  ]

  if (!nrow(fonts)) {
    return(NULL)
  }

  # an oblique face can name its slant in the style and still report italic
  # FALSE, which would let it fill the upright slot
  is_italic <- fonts$italic %in%
    TRUE |
    str_detect(fonts$style, regex("italic|oblique", ignore_case = TRUE))
  # the four standard faces name themselves after the slot they fill, and that
  # name is the only thing separating a Regular from the Display or the Black
  # cut sitting beside it, all three reporting weight normal. A family naming
  # them otherwise falls back on the nearest weight, which still beats a bucket
  # lumping every non-bold cut into the upright slot
  named <- tolower(str_remove_all(fonts$style, "\\s"))
  weight <- as.integer(fonts$weight)

  slot <- \(italic, style, target) {
    exact <- fonts$path[named %in% style]

    if (length(exact)) {
      return(exact[[1]])
    }

    near <- which(is_italic == italic)

    if (!length(near)) {
      return(NULL)
    }

    target <- match(target, levels(fonts$weight))

    fonts$path[near[which.min(abs(weight[near] - target))]]
  }

  faces <- list(
    regular = slot(FALSE, "regular", "normal"),
    bold = slot(FALSE, "bold", "bold"),
    italic = slot(TRUE, "italic", "normal"),
    bold_italic = slot(TRUE, "bolditalic", "bold")
  )

  if (is.null(faces$regular)) {
    return(NULL)
  }

  faces <- compact(faces)

  # a family shipping no bold, or a variable font reporting one file under
  # several styles, would otherwise declare an embedded bold drawn from
  # regular outlines, which is what stops Word synthesising one
  faces[!duplicated(unlist(faces))]
}


# An SVG referenced by <img> is an isolated document and never reaches the
# @font-face rules of the page around it, so the face has to travel inside the
# file. WOFF2 as a data URI, never font_face(embed = TRUE): that one re-emits
# the face as uncompressed TTF through FreeType, thirteen times the weight and
# unreadable where brotli is not compiled in.
.web_fonts <- \(family = NULL) {
  family <- family %||% .text_font()
  faces <- .bundled_faces[[tolower(family)]]

  if (is.null(faces)) {
    if (!tolower(family) %in% c("sans", "serif", "mono")) {
      cli_inform(
        message = c(
          "!" = "No bundled face for {.val {family}}: exported SVGs render in
                 whatever the reader has installed.",
          "i" = "Pass {.arg web_fonts} to {.fn easy_out} to supply your own."
        ),
        .frequency = "once",
        .frequency_id = paste0("hebstr_web_fonts_", tolower(family))
      )
    }

    return(NULL)
  }

  dir <- system.file("fonts", package = "hebstr")

  lapply(faces, \(face) {
    svglite::font_face(
      family = family,
      woff2 = paste0(
        "data:font/woff2;base64,",
        xfun::base64_encode(file.path(dir, face[["file"]]))
      ),
      weight = face[["weight"]],
      style = "normal"
    )
  })
}


# The CSS twin of .web_fonts(): a table written to HTML is read on a machine
# that has no reason to hold the family, and its stylesheet is the only thing
# travelling with it. Same faces, same data URI, emitted as rules rather than
# as svglite font_face objects, which serve a device and not a document.
.css_faces <- \(family = NULL) {
  family <- family %||% .text_font()
  faces <- .bundled_faces[[tolower(family)]]

  if (is.null(faces)) {
    return(NULL)
  }

  dir <- system.file("fonts", package = "hebstr")

  map_chr(faces, \(face) {
    glue(
      "@font-face {{",
      "font-family: '{family}';",
      "src: url(data:font/woff2;base64,{data}) format('woff2');",
      "font-weight: {face[['weight']]};",
      "font-style: normal;",
      "}}",
      data = xfun::base64_encode(file.path(dir, face[["file"]]))
    )
  }) |>
    paste(collapse = "\n")
}


# svglite resolves the generic aliases through systemfonts, so text naming no
# family (a character `shape` in geom_point, drawn with the device default)
# lands on whatever fontconfig returns rather than on the requested font.
.device_fonts <- \(family = NULL) {
  family <- family %||% .text_font()

  # aliasing a generic to itself asks svglite for a family literally named
  # "sans", which no system provides
  if (tolower(family) %in% c("sans", "serif", "mono")) {
    return(list())
  }

  list(sans = family)
}


#' Standardized GT table theme
#'
#' Applies the package's house style to a [gt::gt()] table: fonts, borders,
#' row striping, heading alignment, and footnote formatting. Numeric columns
#' (stats, estimates, p-values) receive a dedicated font and reduced sizes.
#'
#' @param x A [gt::gt()] table object to style.
#' @param width Table width, in pixels. When `NULL`, the table uses its
#'   natural width.
#' @param alpha Font family for the table text.
#' @param digit Font family applied to numeric columns (stats, estimates,
#'   p-values).
#' @param base Base color for text and body borders.
#' @param color Table background color, also used for the row-striping band.
#' @param bg Background color for the heading, column labels, striping, and
#'   footnotes.
#' @param row_padding Vertical padding of data rows, in pixels.
#' @param title_align Horizontal alignment of the heading (`"justify"`,
#'   `"left"`, `"center"`, or `"right"`). Applied to the title itself, so it
#'   holds even under the `docx = FALSE` refinements, which justify the
#'   footnotes only.
#' @param font_size Base font size, in pixels.
#' @param title_font_size Heading title font size, in pixels.
#' @param stat_font_size Font size of stat and estimate cells, in pixels.
#' @param pvalue_font_size Font size of p-value cells, in pixels.
#' @param row_strip Whether to color the striped rows. When `FALSE`, striping
#'   is made transparent.
#' @param footnote_marks Footnote mark style passed to [gt::tab_options()]
#'   (e.g. `"extended"`, `"standard"`, `"numbers"`).
#' @param footnote_font_size Footnote font size, in pixels.
#' @param footnote_padding Footnote padding, in pixels.
#' @param docx If `TRUE`, skip the cell-level style refinements (aligned
#'   title, justified footnotes, digit font, reduced stat and p-value sizes)
#'   that render poorly in Word output. `title_align` still reaches the
#'   heading through [gt::tab_options()], whose call sits outside the branch.
#'   Defaults to `getOption("hebstr.docx", FALSE)`.
#' @param ... Additional options forwarded to [gt::tab_options()].
#'
#' @return A [gt::gt()] table object (`gt_tbl`).
#' @export
#'
#' @examples
#' set_opts()
#'
#' tbl <- gt::gt(head(penguins)) |>
#'   theme_gt()
#'
theme_gt <- \(
  x,
  width = NULL,
  alpha = check_opts(font$alpha),
  digit = check_opts(font$digit),
  base = "#333333",
  color = check_opts(color$cold[1]),
  bg = "white",
  row_padding = 8,
  title_align = "justify",
  font_size = 13,
  title_font_size = font_size + 1,
  stat_font_size = font_size - 1,
  pvalue_font_size = font_size - 2,
  row_strip = TRUE,
  footnote_marks = "extended",
  footnote_font_size = font_size - 2,
  footnote_padding = row_padding,
  docx = .is_docx(),
  ...
) {
  if (!is_bool(docx)) {
    cli_abort("{.arg docx} must be logical.")
  }

  .f <- \(str) str_subset(names(x$`_data`), str)

  if (!row_strip) {
    color <- "#ffffff00"
  }

  x <- tab_options(
    data = x,
    table.width = width,
    table.font.names = .font_stack(alpha),
    table.font.size = px(font_size),
    table.font.color = base,
    table.background.color = color,
    heading.align = title_align,
    heading.background.color = bg,
    heading.title.font.size = px(title_font_size),
    heading.border.bottom.style = "none",
    heading.padding = px(10),
    column_labels.border.top.style = "none",
    column_labels.border.bottom.width = px(1),
    column_labels.border.bottom.color = base,
    column_labels.background.color = bg,
    table.border.top.style = "none",
    table.border.bottom.style = "none",
    table_body.border.top.width = px(1),
    table_body.border.top.color = base,
    table_body.border.bottom.width = px(1),
    table_body.border.bottom.color = base,
    table_body.hlines.style = "none",
    container.height = pct(100),
    container.width = pct(100),
    data_row.padding = px(row_padding),
    data_row.padding.horizontal = px(5),
    row.striping.include_table_body = TRUE,
    row.striping.background_color = bg,
    footnotes.marks = footnote_marks,
    footnotes.font.size = px(footnote_font_size),
    footnotes.padding = px(footnote_padding),
    footnotes.background.color = bg,
    ...
  )

  if (!docx) {
    x <- x |>
      tab_style(
        style = cell_text(align = title_align),
        locations = cells_title()
      ) |>
      tab_style(
        style = cell_text(align = "justify"),
        locations = cells_footnotes()
      ) |>
      tab_style(
        style = cell_text(font = .font_stack(digit)),
        locations = cells_body(columns = .f("stat|estimate|p.value"))
      ) |>
      tab_style(
        style = cell_text(size = px(stat_font_size)),
        locations = cells_body(columns = .f("stat|estimate"))
      ) |>
      tab_style(
        style = cell_text(size = px(pvalue_font_size)),
        locations = cells_body(columns = .f("p.value"))
      )
  }

  return(x)
}


# Also the base the Word branch of tbl_format() sizes the dm column from, so the
# default has to be readable outside the signature.
.ft_size <- 9

# Also what tbl_caption() gives a caption set after the theme has run.
.ft_title <- list(size = 10, color = "#111111")


#' Standardized flextable theme
#'
#' Applies the package's house style to a [flextable::flextable()] table, the
#' Word-facing twin of [theme_gt()]: fonts, borders, row striping, caption
#' alignment, and footnote formatting. Numeric columns (stats, estimates,
#' p-values) receive reduced sizes, and the `digit` font when it is passed.
#'
#' Sizes are in points, the unit `flextable` writes to OOXML, where [theme_gt()]
#' takes pixels; the defaults are tuned for Word rather than converted from the
#' `gt` values. Footnote marks have no counterpart here: they are the ones
#' [gtsummary::as_flex_table()] assigns at conversion.
#'
#' The caption is set bold, in `alpha` at `title_font_size`, as direct
#' formatting: the `Table Caption` style it keeps would otherwise take the
#' typography of whatever template writes the document. [tbl_caption()] gives
#' the same typography to a caption set after the theme. A spanner row is ruled
#' under its labels only, as `gt` does, and the column labels sit at the bottom
#' of their cells.
#'
#' The fonts default to Aptos rather than to the session font of [set_opts()]:
#' a Word table is read on a machine that holds the Office families and rarely
#' anything else. [easy_out()] declares Calibri as its fallback.
#'
#' @param x A [flextable::flextable()] table object to style.
#' @param width Table width, as a fraction of the available page width, from 0
#'   to 1. When `NULL`, the table keeps its natural width.
#' @param alpha Font family for the table text. Defaults to `"Aptos"`, whatever
#'   the session font.
#' @param digit Font family applied to numeric columns (stats, estimates,
#'   p-values). Defaults to `alpha`.
#' @param base Base color for text and body borders.
#' @param color Table background color, also used for the row-striping band.
#' @param bg Background color for the column labels, striping, and footnotes.
#' @param row_padding Vertical padding of data rows, in points.
#' @param title_align Horizontal alignment of the caption (`"left"`,
#'   `"center"`, `"right"`, or `"justify"`).
#' @param title_font_size Caption font size, in points.
#' @param font_size Base font size, in points.
#' @param stat_font_size Font size of stat and estimate cells, in points.
#' @param pvalue_font_size Font size of p-value cells, in points.
#' @param row_strip Whether to color the striped rows. When `FALSE`, the
#'   striping band is made transparent.
#' @param footnote_font_size Footnote font size, in points.
#' @param footnote_padding Footnote padding, in points.
#' @param repeat_header Whether Word repeats the header rows at the top of each
#'   page a split table runs onto. Defaults to `FALSE`, so the continuation
#'   opens on its data rows. A `repeat_headers` entry of `opts_word` passed
#'   through `...` takes precedence. The option is set by the same call to
#'   [flextable::set_table_properties()] as `width`: a later call to that
#'   function resets it to the `flextable` default, which repeats.
#' @param ... Additional properties forwarded to
#'   [flextable::set_table_properties()].
#'
#' @return A [flextable::flextable()] object.
#' @export
#'
#' @examples
#' set_opts()
#'
#' ft <- flextable::flextable(head(penguins)) |>
#'   theme_ft()
#'
theme_ft <- \(
  x,
  width = NULL,
  alpha = .font_fallback[[1]],
  digit = alpha,
  base = "#333333",
  color = check_opts(color$cold[1]),
  bg = "white",
  row_padding = 3,
  title_align = "left",
  title_font_size = .ft_title$size,
  font_size = .ft_size,
  stat_font_size = font_size - 1,
  pvalue_font_size = font_size - 2,
  row_strip = TRUE,
  footnote_font_size = font_size - 2,
  footnote_padding = row_padding,
  repeat_header = FALSE,
  ...
) {
  if (!is_bool(repeat_header)) {
    cli_abort("{.arg repeat_header} must be `TRUE` or `FALSE`.")
  }

  .f <- \(str) str_subset(x$col_keys, str)

  if (!row_strip) {
    color <- "transparent"
  }

  rule <- fp_border(color = base, width = 1)
  # gt leaves its odd rows on the table colour and whitens the even ones
  stripe <- which(seq_len(nrow(x$body$dataset)) %% 2 == 0)

  x <- x |>
    border_remove() |>
    font(fontname = alpha, part = "all") |>
    font(fontname = digit, j = .f("stat|estimate|p.value"), part = "body") |>
    fontsize(size = font_size, part = "all") |>
    fontsize(size = stat_font_size, j = .f("stat|estimate"), part = "body") |>
    fontsize(size = pvalue_font_size, j = .f("p.value"), part = "body") |>
    fontsize(size = footnote_font_size, part = "footer") |>
    flextable::color(color = base, part = "all") |>
    flextable::bg(bg = color, part = "body") |>
    flextable::bg(i = stripe, bg = bg, part = "body") |>
    flextable::bg(bg = bg, part = "header") |>
    flextable::bg(bg = bg, part = "footer") |>
    hline_bottom(border = rule, part = "header") |>
    hline_top(border = rule, part = "body") |>
    hline_bottom(border = rule, part = "body") |>
    padding(
      padding.top = row_padding,
      padding.bottom = row_padding,
      part = "body"
    ) |>
    padding(
      padding.top = footnote_padding,
      padding.bottom = footnote_padding,
      part = "footer"
    ) |>
    align(align = "justify", part = "footer") |>
    valign(valign = "bottom", part = "header") |>
    .ft_spanner_rule(border = rule)

  if (!is.null(x$caption$value)) {
    x <- .ft_caption(
      x,
      .caption_text(x$caption),
      font = alpha,
      align = title_align,
      size = title_font_size
    )
  }

  props <- list2(...)

  if (!is.null(width)) {
    props$layout <- props$layout %||% "autofit"
    props$width <- width
  }

  props$opts_word$repeat_headers <- props$opts_word$repeat_headers %||%
    repeat_header

  x <- inject(set_table_properties(x, !!!props))

  return(x)
}


.ft_spanner_rule <- \(x, border) {
  chunks <- information_data_chunk(x)
  chunks <- chunks[chunks$.part == "header" & nzchar(trimws(chunks$txt)), ]

  reduce(
    seq_len(max(nrow_part(x, "header") - 1, 0)),
    \(ft, i) {
      start <- unique(match(chunks$.col_id[chunks$.row_id == i], ft$col_keys))
      width <- pmax(ft$header$spans$rows[i, start], 1)
      j <- unique(unlist(map2(start, width, \(s, w) seq(s, length.out = w))))

      if (length(j) == 0) {
        return(ft)
      }

      hline(ft, i = i, j = j, border = border, part = "header")
    },
    .init = x
  )
}

.ft_caption <- \(x, caption, font, align, size = .ft_title$size) {
  set_caption(
    x,
    caption = as_paragraph(as_chunk(
      caption,
      props = fp_text(
        font.family = font,
        font.size = size,
        bold = TRUE,
        color = .ft_title$color
      )
    )),
    fp_p = fp_par(text.align = align, padding = 3)
  )
}

# a caption already styled is stored as a chunk frame, a plain one as a string
.caption_text <- \(caption) {
  if (isTRUE(caption$simple_caption)) {
    return(as.character(caption$value))
  }

  str_flatten(caption$value$txt)
}

.ft_font <- \(x) {
  fonts <- c(
    x$header$styles$text$font.family$data,
    x$body$styles$text$font.family$data
  )

  if (length(fonts) == 0) .font_fallback[[1]] else fonts[[1]]
}


# Also the unit the get_vars_dict() column widths are counted in, so the default
# has to be readable from both signatures.
.rt_size <- "0.65rem"


#' Standardized reactable widget theme
#'
#' Builds the package's house palette for a [reactable::reactable()] widget, the
#' HTML-only member of the table themes. Unlike [theme_gt()] and [theme_ft()],
#' which take a table and return a table, this one takes no table and returns a
#' value: `reactable` reads its theme at construction and offers no way to
#' restyle a widget afterwards. Pass it at the call site,
#' `reactable(theme = theme_rt())`, or once for a session,
#' `options(reactable.theme = theme_rt())`, which [reactable::reactable()]
#' reads as its own default.
#'
#' The color defaults are CSS custom properties carrying a literal fallback,
#' which `reactable` serialises to the DOM as written. Under a document
#' declaring `--primary-surface` and `--primary-back` (the `hebstr-doc` Quarto
#' theme does) the widget follows the light and dark schemes; anywhere else,
#' the standalone artifacts [easy_out()] writes included, every token falls
#' back to its literal and the rendering is the package palette. Ground and
#' striping are the inverse of [theme_gt()], which tints its body and leaves
#' its striping white: a `reactable` follows the ordinary convention of a white
#' ground and a tinted stripe.
#'
#' @param alpha Font family for the widget's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param font_size CSS font size for the widget's text. [get_vars_dict()]
#'   counts its column widths in this size, so a theme carrying another one
#'   resizes them too.
#' @param bg Background color of the widget. `reactable` paints `.Reactable`
#'   white in its own stylesheet, so a widget left without this renders as a
#'   white card carrying light text under a dark scheme.
#' @param strip_color Background color of the striped rows. The fallback is the
#'   first cold-palette color from the options.
#' @param border_color Color of the cell rules. A translucent mix rather than a
#'   token, so it composes on whichever surface carries it and follows the
#'   scheme on its own.
#' @param expander_color Color of the group expander arrow, which `reactable`
#'   paints at `rgba(0, 0, 0, 0.8)` and no [reactable::reactableTheme()]
#'   argument reaches: it goes in through a nested selector key instead. Never
#'   `currentColor`, which resolves to `transparent` here, `reactable` setting
#'   `color: transparent` on the element carrying the arrow.
#' @param search_style Style of the search input, forwarded to
#'   [reactable::reactableTheme()].
#' @param ... Additional arguments forwarded to [reactable::reactableTheme()].
#'
#' @return A [reactable::reactableTheme()] object.
#' @export
#'
#' @examples
#' theme_rt()
#'
#' theme_rt(strip_color = "#F5FBFF")
#'
theme_rt <- \(
  alpha = .text_font(),
  font_size = .rt_size,
  bg = "var(--primary-surface, #ffffff)",
  strip_color = str_c(
    "var(--primary-back, ",
    set_opts(.assign = FALSE)$color$cold[1],
    ")"
  ),
  border_color = "color-mix(in srgb, currentColor, transparent 88%)",
  expander_color = "var(--bs-body-color, rgba(0, 0, 0, 0.8))",
  search_style = list(width = "100%"),
  ...
) {
  reactableTheme(
    style = list(
      fontSize = font_size,
      fontFamily = .font_css(alpha),
      ".rt-expander:after" = list(borderTopColor = expander_color)
    ),
    backgroundColor = bg,
    stripedColor = strip_color,
    borderColor = border_color,
    searchInputStyle = search_style,
    ...
  )
}


#' Standardized bar-chart theme
#'
#' A [ggplot2::theme()] built on textbox title and caption, tuned for the
#' package's bar charts ([ggcount()]).
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param text_color Color of the theme's text.
#' @param title_size Plot title font size, in points.
#' @param title_halign Horizontal alignment of the title text box, from 0
#'   (left) to 1 (right).
#' @param title_margin Margin around the title, from [ggplot2::margin()].
#' @param caption_size Caption font size, in points.
#' @param caption_halign Horizontal alignment of the caption text box, from 0
#'   (left) to 1 (right).
#' @param caption_margin Margin around the caption, from [ggplot2::margin()].
#' @param grid Whether to keep the panel background. When `FALSE`, the panel
#'   background and strip text are blanked and axis lines are drawn.
#' @param legend_position Legend position passed to [ggplot2::theme()].
#' @param ... Additional theme elements merged in via `%+replace%`.
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(penguins, aes(species)) +
#'   geom_bar() +
#'   theme_bar()
#'
theme_bar <- \(
  family = .text_font(),
  text_color = "#333333",
  title_size = 9,
  title_halign = 1,
  title_margin = margin(0, 0, 0, 0),
  caption_size = 9,
  caption_halign = 0,
  caption_margin = margin(10, 0, 0, 0),
  grid = TRUE,
  legend_position = "none",
  ...
) {
  if (!grid) {
    bg <- list(
      panel.background = element_blank(),
      axis.line = element_line(),
      strip.text = element_blank()
    )
  } else {
    bg <- NULL
  }

  theme(
    text = element_text(family = family, color = text_color),
    plot.title = element_textbox(
      size = title_size,
      halign = title_halign,
      margin = title_margin,
      lineheight = 1.05,
      width = unit(1, "npc")
    ),
    plot.caption.position = "plot",
    plot.caption = element_textbox(
      size = caption_size,
      halign = caption_halign,
      margin = caption_margin,
      lineheight = 1.05,
      width = unit(1, "npc")
    ),
    axis.title = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(vjust = 0.5),
    legend.position = legend_position
  ) %+replace%
    inject(theme(!!!bg, ...))
}


#' Time-to-event (survival) plot theme
#'
#' A [ggplot2::theme_classic()]-based theme for Kaplan-Meier and other
#' time-to-event curves, with thin lines and a caption text box.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param size Caption font size, in points.
#' @param vjust_y Vertical justification of the left y-axis title.
#' @param title_margin Margin around the caption, from [ggplot2::margin()].
#' @param ... Additional elements passed to [ggplot2::theme()].
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(na.omit(penguins), aes(flipper_len, body_mass, color = species)) +
#'   geom_line() +
#'   theme_tte()
#'
theme_tte <- \(
  family = .text_font(),
  size = 8,
  vjust_y = 1,
  title_margin = NULL,
  ...
) {
  theme_classic() %+replace%
    theme(
      line = element_line(linewidth = 0.3),
      text = element_text(family = family),
      axis.title = element_text(face = "bold", size = 8),
      axis.title.x = element_text(vjust = -1),
      axis.title.y.left = element_text(vjust = vjust_y),
      axis.text = element_text(size = 7),
      panel.background = element_blank(),
      plot.background = element_blank(),
      plot.margin = margin(0, 0, 0, 0),
      plot.caption = element_textbox(
        size = size,
        width = unit(1, "npc"),
        margin = title_margin
      ),
      plot.caption.position = "plot",
      legend.position = "none",
      ...
    )
}


#' Risk-table theme for survival plots
#'
#' Styles the risk table accompanying a time-to-event plot, layering package
#' overrides on top of `theme_risktable_default()`.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param label_size Font size of the y-axis (strata) labels, in points.
#' @param title_size Plot title font size, in points.
#' @param title_margin Base size, in points, of the title margin (applied as a
#'   negative top and positive bottom offset).
#' @param plot_margin Base size, in points, of the plot margin (applied as a
#'   positive top and negative bottom offset).
#' @param ... Additional elements passed to [ggplot2::theme()].
#'
#' @return A list of ggplot2 theme components.
#' @export
#'
#' @examples
#' \donttest{
#' library(ggsurvfit)
#'
#' survfit2(Surv(time, status) ~ sex, data = df_lung) |>
#'   ggsurvfit() +
#'   add_risktable(theme = theme_risktable())
#' }
#'
theme_risktable <- \(
  family = .text_font(),
  label_size = 7,
  title_size = 7,
  title_margin = 3,
  plot_margin = 10,
  ...
) {
  .title_margin <- margin(-title_margin, 0, title_margin, 0)
  .plot_margin <- margin(plot_margin, 0, -plot_margin, 0)

  list(
    theme_risktable_default(),
    theme(
      text = element_text(family = family),
      plot.title = element_text(
        size = title_size,
        face = "bold",
        margin = .title_margin
      ),
      panel.background = element_blank(),
      plot.background = element_blank(),
      axis.text.y = element_markdown(size = label_size),
      plot.margin = .plot_margin,
      plot.title.position = "plot",
      ...
    )
  )
}


#' PCA / ordination plot theme
#'
#' A minimal [ggplot2::theme()] for principal-component and other ordination
#' plots: axis text, titles, and ticks are blanked, leaving a caption text box.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param ... Additional theme elements merged in via `%+replace%`.
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' num <- c("bill_len", "bill_dep", "flipper_len", "body_mass")
#' pca <- prcomp(na.omit(penguins)[num], scale. = TRUE)
#'
#' p <- ggplot(as.data.frame(pca$x), aes(PC1, PC2)) +
#'   geom_point() +
#'   theme_pca()
#'
theme_pca <- \(family = .text_font(), ...) {
  theme(
    text = element_text(family = family),
    legend.position = "none",
    plot.caption = element_textbox(
      size = 9,
      hjust = 1,
      lineheight = 1.05,
      width = unit(1, "npc"),
      margin = margin(10, 0, 0, 0)
    ),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    plot.caption.position = "plot"
  ) %+replace%
    theme(...)
}


#' Blank canvas theme
#'
#' A [ggplot2::theme_void()]-based theme with a white background and textbox
#' title and caption, optionally adding horizontal grid lines and y-axis text.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param grid Whether to draw major horizontal grid lines and y-axis text.
#' @param grid_color Color of the grid lines and y-axis text.
#' @param axis_text_size_y Font size of the y-axis text, in points.
#' @param grid_size Line width of the grid lines.
#' @param legend_position Legend position passed to [ggplot2::theme()].
#' @param ... Additional theme elements merged in via `%+replace%`.
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(penguins, aes(species, fill = species)) +
#'   geom_bar() +
#'   theme_blank(grid = TRUE)
#'
theme_blank <- \(
  family = .text_font(),
  grid = FALSE,
  grid_color = "grey90",
  grid_size = 0.2,
  axis_text_size_y = 7,
  legend_position = "none",
  ...
) {
  .blank <- element_rect(color = "white", fill = "white")

  .width <- unit(1, "npc")

  grid <- if (grid) {
    list(
      panel.grid.major.y = element_line(
        color = grid_color,
        linewidth = grid_size
      ),
      axis.text.y = element_text(color = grid_color, size = axis_text_size_y)
    )
  } else {
    NULL
  }

  theme_void() %+replace%
    theme(
      plot.background = .blank,
      panel.background = .blank,
      plot.margin = margin(0, 5, 5, 5),
      plot.title = element_textbox(
        size = 9,
        width = .width,
        margin = margin(0, 0, 0, 0)
      ),
      plot.caption = element_textbox(
        size = 9,
        hjust = 1,
        lineheight = 1.05,
        width = .width,
        margin = margin(10, 0, 0, 0)
      ),
      text = element_text(family = family),
      legend.position = legend_position,
      ...
    ) %+replace%
    theme(!!!grid)
}


#' Ranked-frequency bar theme
#'
#' A [ggplot2::theme()] for horizontal ranked-frequency bar charts: a centered
#' markdown title, right-aligned caption, blanked axis titles, and optional
#' vertical grid lines.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param title_size Plot title font size, in points.
#' @param title_margin Bottom margin below the title, in points.
#' @param caption_size Caption font size, in points.
#' @param caption_margin Top margin above the caption, in points.
#' @param label_size Font size of the y-axis labels, in points.
#' @param label_margin Margin around the y-axis labels, from [ggplot2::margin()].
#' @param grid Whether to draw major vertical grid lines and x-axis text.
#' @param grid_size Font size of the x-axis text drawn with the grid, in points.
#' @param ... Additional theme elements merged in via `%+replace%`.
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(penguins, aes(y = species)) +
#'   geom_bar() +
#'   theme_infreq()
#'
theme_infreq <- \(
  family = .text_font(),
  title_size = 11,
  title_margin = 10,
  caption_size = 9,
  caption_margin = 10,
  label_size = 11,
  label_margin = margin(r = -15),
  grid = TRUE,
  grid_size = 7,
  ...
) {
  if (grid) {
    grid <- list(
      panel.grid.major.x = element_line(color = "grey95", linewidth = 0.3),
      axis.text.x = element_text(
        color = "grey90",
        size = grid_size,
        margin = margin(0)
      )
    )
  } else {
    grid <- NULL
  }

  theme(
    plot.title.position = "plot",
    plot.title = element_markdown(
      size = title_size,
      color = "#333333",
      hjust = 0.5,
      halign = 0.5,
      margin = margin(0, 0, title_margin, 0)
    ),
    plot.caption = element_markdown(
      size = caption_size,
      lineheight = 1.05,
      hjust = 1,
      halign = 1,
      margin = margin(caption_margin, 0, 0, 0)
    ),
    axis.title = element_blank(),
    axis.text.y = element_text(size = label_size, margin = label_margin),
    text = element_text(family = family),
    plot.caption.position = "plot",
    legend.position = "none",
    panel.background = element_blank(),
    axis.line = element_blank(),
    strip.text = element_blank(),
    axis.ticks = element_blank(),
    axis.text.x = element_blank(),
    ...
  ) %+replace%
    theme(!!!grid)
}


#' Bubble-chart theme
#'
#' A [ggplot2::theme()] for bubble charts, with markdown axis text and titles
#' and per-axis control over text, title, and grid colors. Axis colors may be
#' vectors, letting the title and grid colors adapt to a colored axis.
#'
#' @param family Font family for the theme's text. When [set_opts()] has been
#'   called, defaults to the centralised text font (`opts$font$alpha`);
#'   otherwise the OS-agnostic system sans-serif (`"sans"`).
#' @param size Base text size, in points.
#' @param base_color Default color for text, titles, and borders.
#' @param axis_margin_x Top margin of the x-axis title, in points.
#' @param axis_margin_y Right margin of the y-axis title, in points.
#' @param axis_color_x Color(s) of the x-axis text; `NA` is transparent.
#' @param axis_color_y Color(s) of the y-axis text; `NA` is transparent.
#' @param title_color_x Color of the x-axis title; defaults to `axis_color_x`
#'   when it is a single color, otherwise `base_color`.
#' @param title_color_y Color of the y-axis title; defaults to `axis_color_y`
#'   when it is a single color, otherwise `base_color`.
#' @param grid_color_x Color of the vertical grid lines; defaults to
#'   `axis_color_x` when it differs from `base_color`, otherwise `"grey95"`.
#' @param grid_color_y Color of the horizontal grid lines; defaults to
#'   `axis_color_y` when it differs from `base_color`, otherwise `"grey95"`.
#' @param grid_lighten_x Amount to lighten `grid_color_x`, from 0 to 1;
#'   `0.85` for any grid color other than the default grey, which takes `0`.
#' @param grid_lighten_y Amount to lighten `grid_color_y`, from 0 to 1;
#'   `0.85` for any grid color other than the default grey, which takes `0`.
#' @param ... Additional elements passed to [ggplot2::theme()].
#'
#' @return A [ggplot2::theme()] object.
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(na.omit(penguins), aes(bill_len, body_mass, size = flipper_len)) +
#'   geom_point(alpha = 0.5) +
#'   theme_bubble()
#'
theme_bubble <- \(
  family = .text_font(),
  size = 13,
  base_color = "#333333",
  axis_margin_x = 12,
  axis_margin_y = 10,
  axis_color_x = base_color,
  axis_color_y = base_color,
  title_color_x = if (length(axis_color_x) == 1) axis_color_x else base_color,
  title_color_y = if (length(axis_color_y) == 1) axis_color_y else base_color,
  grid_color_x = if (!any(axis_color_x %in% base_color)) {
    axis_color_x
  } else {
    "grey95"
  },
  grid_color_y = if (!any(axis_color_y %in% base_color)) {
    axis_color_y
  } else {
    "grey95"
  },
  grid_lighten_x = if (!any(grid_color_x %in% "grey95")) 0.85 else 0,
  grid_lighten_y = if (!any(grid_color_y %in% "grey95")) 0.85 else 0,
  ...
) {
  theme(
    panel.background = element_blank(),
    panel.grid.major.x = element_line(
      color = lighten(grid_color_x, grid_lighten_x)
    ),
    panel.grid.major.y = element_line(
      color = lighten(grid_color_y, grid_lighten_y)
    ),
    text = element_text(size = size, family = family),
    axis.title = element_markdown(face = "bold"),
    axis.title.x = element_markdown(
      margin = margin(t = axis_margin_x),
      color = title_color_x
    ),
    axis.title.y = element_markdown(
      margin = margin(l = 20, r = axis_margin_y),
      color = title_color_y
    ),
    axis.text.x = element_markdown(color = axis_color_x),
    axis.text.y = element_markdown(color = axis_color_y),
    axis.ticks = element_blank(),
    legend.position = "none",
    ...
  )
}
