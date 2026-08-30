# hebstr 0.0.0.9000

## Breaking changes

Package state no longer touches the user's workspace.
Options and the variable classification cache now live in an internal package store instead of the global environment, so the package complies with the CRAN policy against writing to `.GlobalEnv`.

- `easy_out_map()` is removed, its behaviour folded into `easy_out()`.
  Hand `easy_out()` a named list and it writes the elements one by one, exactly as the map did: the element name follows `filename` after `sep`, and the elements share the folder the list derives.
  `easy_out_map(plots, filename = "fig")` becomes `easy_out(plots, filename = "fig")`; nothing else about the call or the files it writes changes.
  The map added no step of its own, only a second class guard deciding "a list of outputs" against "an output that is a list" for a set where four of the six supported classes are named lists: `gt_tbl`, `gtsummary`, `htmlwidget` and `hebstr_dict`.
  That guard is now a single branch of `easy_out()`, and the special case that pointed a `hebstr_dict` back at `easy_out()` is gone with it.
  The fallback takes a bare list alone, so an object carrying a class of its own is still one output; a `flextable` goes down its own branch rather than being walked over.
  The banner also stops announcing `Object: data` for every element, `data` having been the name of the map's own loop variable, and names the element instead.
- `easy_out()` writes each output into a folder of its own inside `dir`, and names both the folder and the files in kebab-case.
  `easy_out(fig_surv_strata)` writes `output/fig-surv-strata/fig-surv-strata.svg` where it wrote `output/fig_surv_strata.svg`.
  The rule is `filename` lowercased, with every run of non-alphanumeric characters folded into a single dash: `sep` therefore separates without surviving verbatim, and a `filename` that cleaning leaves empty is an error rather than a silent fallback.
  The folder is derived before `suffix` is appended, so the variants of one output share it: `output/tbl-baseline/tbl-baseline-v2.html`.
  For a named list the folder is derived once from the base name, so the elements stay grouped instead of scattering one folder per element.
  The directory served over HTTP in a remote session is unchanged: the root stays `dir`, the folder coming through as a path segment of the URL.
  The new `subdir` argument takes a string to name the folder, or `FALSE` to write straight into `dir` as before; the file names are kebab-case either way.
  The break is silent rather than noisy: the files earlier runs wrote flat stay where they are, nothing errors, and a hard-coded path such as `knitr::include_graphics("output/fig_flowchart.svg")` keeps resolving, now to an artefact no run refreshes.
  A project catches up by deleting the old contents of its output directory and pointing its hard-coded paths at the new folder and file names.
- `easy_out()` requires a `filename` when the object is not passed by name.
  The default derived it from a deparse of the expression, so the callee entered the name of both the folder and the file: `easy_out(get_xlsx(checklist))` wrote `get-xlsx-checklist/`, and a piped chain wrote itself out backwards, `df |> tbl_summary() |> tbl_format() |> easy_out()` giving `tbl-format-gtsum-format-tbl-summary-df/`.
  A name that cleaning did not empty passed unremarked, the banner presenting it as a legitimate one, and each way of writing the same call created a folder of its own.
  The abort names the folder that would have been created and points at `filename`; assigning the object first works just as well.
  It fires after the class guards, so an unsupported object is still reported as such, and before the directory is created.
  The banner's `Object:` line now shows the expression handed to `x` rather than the resolved filename, which the lines below it already carry.
- `gt_qmd()` no longer gives every table the same HTML id.
  Its `id` argument defaulted to `"tbl-id"`, a constant, where [gt::gt()] defaults to `NULL` and generates a random unique one.
  That id scopes gt's own stylesheet, so every table in a document shared a single scope and, at equal specificity, the last stylesheet won for all of them: a table customised through `...` either had its settings discarded or leaked them onto its neighbours, depending on source order, in both cases without a warning.
  Measured on a document holding 13 tables, where the one styled at `font_size = 13` rendered at 15 like the rest.
  An explicit `id` is unaffected; only the default changes.
  A document hooking `#tbl-id` from hand-written CSS has to target the Quarto crossref label instead, which is the anchor meant to be addressed.
- `add_code_file()` is removed.
  It generated a Quarto `add-from` directive, a syntax the `hebstr-doc` extension dropped in its 0.10.0 release in favour of the `{{< script path/to/file.R >}}` shortcode, so its output no longer rendered anywhere.
  The shortcode covers the same arguments (`lang`, `filename`, `numbers`, `lines`, `dedent`, `suffix`), including programmatic generation: Quarto processes a shortcode emitted from an R chunk with `results = "asis"`.
  `include_code_file()` is unaffected, its `include=` directive being native Quarto syntax.
- `tbl_format()` takes the table width through its own `width` argument, in pixels on both output branches, instead of forwarding it through `...`.
  Forwarded, `width` reached `theme_gt()` in pixels and `theme_ft()` as a fraction of the page, so no single call site could serve both formats: a pixel value aborted the Word branch on `flextable::set_table_properties()` with "width is > 1".
  The Word branch now converts the pixel width to a fraction of `page_width`, capped at `1`, that width being measured on the `reference-doc` in use and falling back to `6.5` inches, a US Letter page with one-inch margins.
  `width` defaults to `700` pixels; pass `NULL` to keep the table's natural width.
  Code passing a fraction for the Word branch must switch to pixels.
- `set_opts()` no longer creates an `opts` object in the global environment; it stores the options in the internal package store.
  Read the whole object with the new `get_opts()`, or a single validated key with `check_opts()`.
- `get_vars_dict()` loses its `assign` argument (and the `name` argument, which only served to name the assigned widget).
  It returns the dictionary; assign it yourself if you need a bound name.
- `clear_vars()` loses its `env` argument and is now called with no arguments; it always clears the internal cache set by `use_vars()`.
- The default text and numeric font is now the portable `"sans"` family instead of Luciole, so output no longer depends on a non-standard font being installed.
  Opt into Luciole with `set_opts(font = "luciole")`.
- The minimum required R version is now 4.5.0 (was 4.1.0), because examples and helpers rely on the `penguins` dataset shipped with base R since 4.5.0.
- `str_fig()` renames its `qmd` argument to `render` and defaults it to `getOption("str_fig.render", TRUE)` (was `FALSE`), so the output targets a Quarto context by default.
  Pass `render = FALSE`, or set `options(str_fig.render = FALSE)`, for the inline font-size formatting.
- `gt_format()` is renamed to `tbl_format()`.
  The function returns a `flextable` under `options(hebstr.docx = TRUE)`, which the `gt_` prefix contradicts.
  `gt_format()` still forwards to `tbl_format()`, with a deprecation warning.
- `theme_gt(docx = )` reads `getOption("hebstr.docx", FALSE)` instead of the `theme_gt.docx` option, which no longer exists.
  A single option, `hebstr.docx`, now drives the whole Word branch.
- `tbl_format()` now folds the automatic missing-value rows into a single `dm` column by default (`collapse_missing = TRUE`), sizing that column with `missing_size` (default `11`).
  Tables with no missing rows, or built with `gtsummary::tbl_summary(missing = "no")`, are left untouched.
  Pass `collapse_missing = FALSE` to keep the native missing rows.
- `add_note()` takes a `gtsummary` table and is called before `tbl_format()`, where it took a `gt_tbl` and was called after.
  It attaches the footnote with `gtsummary::modify_footnote_body()` instead of `gt::tab_footnote()`, so one call site serves both output formats: a `gt` verb applied after `tbl_format()` fails on the Word branch, and appending the footnote to a rendered `flextable` restarts the symbol counter, colliding with the symbols `tbl_format()` has already emitted.
  Declared upstream, `gtsummary` numbers the footnotes in table reading order whatever order they are declared in.
  Move each `add_note()` call above `tbl_format()` in the pipe.
- `easy_view()` is renamed `get_vars_dict()`.
  The function returns a variable dictionary, of which the interactive widget is one of three forms, beside the tibble and the JSON-friendly copy, so a name built on the display described the smallest part of what comes back.
  It joins `get_opts()` and `get_xlsx()` rather than taking one of the family prefixes, on the same reading: the name says which object comes back.
  There is no alias under the old name; a project catches up by renaming its call sites.
- `get_vars_dict()` loses its `level_sep` argument, and the returned `data` flattens the multi-valued cells.
  The values of such a cell, the levels of a factor, the two ends of a range or the three quartiles, are joined with `" ; "`, in the widget and in `data` alike, and the separator is no longer configurable.
  `data$range`, `data$q1_med_q3` and `data$levels` are therefore strings where each was a list column holding a vector.
  Code reading those cells goes through the `json` element, which keeps them as lists.
- `get_vars_dict()` renames the quartile column of its returned `data` from `q1_q2_q3` to `q1_med_q3`, the middle of the three values being the median rather than a second quartile.
  Code reading `view$data$q1_q2_q3` has to follow, and the widget carries the column under the new name as well.

## Deprecated

- `show_single_row()` is deprecated and warns when called.
  It recoded every two-level factor to a 0/1 indicator so `gtsummary` would render each on a single row, but recoding the data to steer the display discards the factor levels the table then has to label.
  Fold the row from the table instead, through the `show_single_row` argument of `gtsummary::tbl_summary()` or `gtsummary::tbl_regression()`, which keeps the factor.
- `gtsum_format(show_single_row = )` is deprecated and warns when supplied.
  It annotated dichotomous variables with their reference level, but keyed the annotation on `var_type == "dichotomous"`, which `broom.helpers` sets on every two-level factor whether or not its levels are folded onto a single row.
  On a table built without `gtsummary::tbl_regression(show_single_row = <var>)` upstream, it therefore labelled the heading row `NA — ref : <level>` and annotated each level in turn.
  Fold the row with `gtsummary::tbl_regression(show_single_row = <var>)`; the reference level then reads from the `label_reference` marker every regression table already carries.
  `model_mv`, `ref_sep` and `ref_no` serve this argument alone and go with it at removal.

## New features

- `easy_out()` writes a `flextable` to a `.docx`, so a table formatted for Word can leave the session as a standalone file instead of only reaching a rendered document.
  The object is what `tbl_format()` already returns under `options(hebstr.docx = TRUE)`, so nothing new is built and no second object is named beside the table: a script declares the target once and its tables come out as Word files.
  The class carries the dispatch, as it does for a workbook, so there is no new argument and no new exported name.
  `export` keeps its meaning and its default: `options(hebstr.docx = TRUE)` alone still writes nothing, because under a Quarto Word render the table belongs in the document rather than in a file of its own.
  Asking for the file is therefore `options(easy_out.export = TRUE)` alongside it, which is how a whole sweep is switched over in one line:
  `withr::with_options(list(hebstr.docx = TRUE, easy_out.export = TRUE), auto_exec())` writes every table of `scripts/` as a `.docx` beside the HTML and PNG of an ordinary run, the per-output folder keeping the three formats together.
  Two consequences worth knowing.
  The table alignment `theme_ft()` posted survives into the file, `save_as_docx()` centring the table otherwise regardless of what the object carries.
  And the title given to `tbl_format(title = )` reaches the file, where the Quarto docx pipeline drops it: the documented advice to caption through `#| tbl-cap:` still holds for a rendered document, not for this export.

- `easy_out()` accepts an htmlwidget, writing it to a self-contained HTML file, and the variable dictionary `get_vars_dict()` returns, writing the widget and its summary together as HTML, XLSX and JSON.
  `get_vars_dict()` classes its return `hebstr_dict`, which is what `easy_out()` dispatches on, so `easy_out(vars_dict)` names its output after the object rather than needing a `filename` for `vars_dict$output`.
  The dictionary gets no PNG: a raster of an interactive widget freezes the search box and the filter row, and stops at the first page, so a summary of three hundred variables would give an image of a hundred rows looking complete.
  The XLSX carries `data` whole even when `cols` restricts the widget, an archive having no reason to be amputated by a display choice, and the sheet is named after the file, capped at the 31 characters Excel allows.
  The JSON carries the `json` element, written with `auto_unbox = TRUE` so that the cells `get_vars_dict()` marks with `I()` come out as arrays even when they hold a single value, and with missing values as `null`.
  The frame is not read back out of the widget: reactable holds it as JSON under a private reactR structure with no exported accessor, encodes a missing value as the string `"NA"` so an all-missing integer column comes back logical, and carries the displayed selection rather than the source.
  A self-contained file needs pandoc, which `htmlwidgets::saveWidget()` reaches through rmarkdown; without it the widget is written beside its library folder and a warning says so.
  `htmlwidgets` and `jsonlite` join `Imports`, both already installed as dependencies of reactable, and `rmarkdown` stays a `Suggests`, reached only through the guarded call above.

- `easy_check()` turns a set of logical checks into a data-quality report: one row per pair of a row and a check it fails, identified by `.id` and carried by whatever context `.with` declares.
  It is meant for the spreadsheet a data manager works from, one line per thing to look at, and goes straight into [get_xlsx()] and [easy_out()].
  `.with` follows the semantics of [dplyr::transmute()]: name what is computed (`n_cures = induc_nb + adj_nb`), give bare what already exists (`centre`), and the columns come out in that order.
  It runs before the checks, so a check reads what it declares, and the same expression is therefore written once rather than computed upstream and named again to be carried.
  A check that reads a missing value decides nothing rather than passing: `.na = "drop"` leaves it out, `.na = "flag"` reports it with a `status` column telling `fail` from `unknown`, and a check is made total at its own site, where the knowledge of whether an absence is legitimate lives.
  `.id` defaults to `"rowname"`, the column [tibble::rownames_to_column()] adds, and its values need not be unique: a repeated identifier is itself a legitimate thing to check for.
  Every check must be named and must evaluate to a logical vector, which is what keeps a `0`/`1` column from being taken for a check that passes everywhere.

- `get_vars_dict(cols = )` chooses which columns of the summary the widget shows, as a tidy-selection: `get_vars_dict(df, cols = c(n, variable, type, n_miss))` drops the labels and the levels from a display that only has to answer what is missing.
  The selection reads the displayed names, `n` rather than the `pos` the returned `data` carries.
  An empty selection is an error rather than a widget of nothing.
  The default is `everything()`, so the widget now also carries `range` and `q1_med_q3`, which it used to drop while the returned `data` kept them: a call left as it was renders two columns wider, and `cols = -matches(c("range", "q1_med_q3"))` restores the former display.

- `easy_out(pptx = TRUE)` also writes a plot or a grid graphic to a PPTX slide, as an editable DrawingML shape rather than an image.
  A project that wanted an editable figure wrote a writer of its own next to `easy_out()`, half of it restating the `dir` default, the filename derivation, the folder creation and the class check, and entering the size a second time with nothing keeping the two calls in step.
  The slide goes into the same folder and takes the same name as the SVG and the PNG, and it is scaled to fit the default 4:3 `Office Theme` slide and centred on it, from the `width` and `height` the call already resolved.
  The default is `FALSE`, read from `getOption("easy_out.pptx")`, so a presentation-oriented project switches every output at once.
  It needs the \pkg{rvg} package, a `Suggests` rather than an `Imports` because it pulls `gdtools` and its cairo, freetype2 and fontconfig system requirements.
  Two deliberate gaps: `crop` does not reach the slide, which carries the whole canvas, an editable object being cropped in the tool that opens it; and fonts are named in the slide rather than embedded, so a reader without the family installed gets a substitute.
  Tables and workbooks have no slide form and error out rather than ignoring the argument.

- `with_fig_device(width, height, code)` evaluates `code` on an off-screen device of the given size, then closes it and restores the device that was current.
  `grid` resolves a unit against the device current at the time of the conversion, and a package that precomputes layout performs it when the grob is created rather than when it is drawn, `Gmisc` among them.
  A flowchart built in the plot pane then carries coordinates measured against the pane, and drawing it on the canvas `easy_out()` opens displaces everything derived from a box edge: `Gmisc::connectGrob(type = "N")` places its horizontal segment halfway between two boxes, and that half-distance shrinks as the construction device gets shorter.
  Build with the width and height `easy_out()` will be called with, and the exported figure stops depending on the pane size or on the `fig-height` of the chunk that happened to source the script.
  The device comes from `svglite::svgstring()`, the export's own family measuring into a memory buffer: `cairo_pdf`, `ragg` and `pdf(NULL)` each measure the same text differently, and `withr::with_svg()` wraps the cairo device.
  A bare symbol passed to `code` is an error, that construction having been forced by its own assignment before the call: the device then opens and closes around a lookup and measures nothing, reinstating in silence the very drift the function exists to remove.
  Splitting the construction out of the call stays possible through a function, `code = .flow_grob()` against `code = .flow_grob`.

- `easy_out()` writes an `openxlsx2` workbook to XLSX, so the object `get_xlsx()` returns reaches disk through the package's export verb rather than a qualified `openxlsx2::wb_save()` call, with the output directory created and the `dir`/`filename`/`suffix` conventions applied.
  The `export` guard keeps reading the session option alone, so a workbook exported during a Word run (`options(hebstr.docx = TRUE)`) needs an explicit `export = TRUE`, as any other object does.

- `tbl_format()` routes Word output through `flextable` instead of `gt`.
  Under `options(hebstr.docx = TRUE)` it returns a themed `flextable` rather than a `gt_tbl`, because `gt` is HTML-first and its OOXML export flattens backgrounds, row striping, and custom borders.
  Acronyms, footnotes, and the zero substitution carry over to that branch; the HTML output is unchanged.

- `tbl_font_size()` and `tbl_row_color()` style table columns and rows on either a `gt_tbl` or a `flextable`, dispatching on the class.
  Downstream styling therefore keeps a single call site across both output formats, where a bare `gt` verb would fail on the Word branch.
  Sizes are given in pixels and converted to points on the `flextable` side.

- `theme_ft()` applies the package house style to a `flextable`, as the Word-facing twin of `theme_gt()`.
  Sizes are in points, not pixels, except `width`, which is a fraction of the available page width; set the table width through `tbl_format(width = )`, in pixels on either branch.
  The refinements `theme_gt(docx = TRUE)` had to drop (justified footnotes, digit font, reduced stat and p-value sizes) render in Word through `flextable`.
  Caption typography is left to the Word `Table Caption` style, its alignment set by `title_align` (default left).

- `easy_out()` gains an `export` argument, reading `getOption("easy_out.export")` and defaulting to `FALSE` under `options(hebstr.docx = TRUE)`.
  A Word run renders tables through `flextable`, leaving no `gt_tbl` to write to HTML or PNG, so the export is skipped rather than aborting on the class.
  The decision is the session's, not the object's: `easy_out()` reads the option and never inspects the class to make it.
  Set `options(easy_out.export = TRUE)` to force the export under `hebstr.docx`, or `FALSE` to silence it in any run.
  Forcing the export writes the `flextable` as a `.docx`, which is how a Word-formatted table leaves the session as a standalone file.

- `easy_out()` accepts a grid grob (for example a Gmisc flowchart built from `boxGrob()`/`connectGrob()`), exporting it to SVG and PNG through the same pipeline as ggplot objects.

- `easy_out()` gains a `crop` argument, reading `getOption("easy_out.crop")` and defaulting to `TRUE`, which trims the SVG canvas of a grid grob to the bounding box of the drawing plus a 3% margin.
  Grob positions are relative to the whole page, so a flowchart covering a sub-rectangle leaves an empty band that no `width`/`height` value removes, both being scaled by the same amount.
  The trim rewrites the SVG `width`, `height`, and `viewBox` on the measured bounding box, a lossless vector operation that leaves text sizes, boxes, and internal spacing untouched; `width` and `height` therefore become a canvas budget rather than the size of the exported file.
  Plots are left alone, their margins coming from the theme.

- `easy_out()` opens its output in the local browser from a remote session.
  The exported file lives on the server while the browser runs on the local machine, so `browseURL()` on a file path could not reach it; the output directory is now served over HTTP on the loopback interface and the URL `http://localhost:<port>/<file>` is opened instead, which the IDE forwards.
  The route is read from `getOption("easy_out.serve")`: `NULL` (the default) detects a remote session through `SSH_CONNECTION`, `TRUE` or `FALSE` force the HTTP or the file-path route, leaving a local session on its former behaviour, and any other value aborts.
  One server is started per session, restarted whenever `dir` or the requested port changes, on a free port unless `getOption("easy_out.port")` pins one; should it fail to start, the file path is opened as before.
  The server is released when the namespace is unloaded, so reloading the package does not strand the port.

- `col_missing()` folds the automatic `gtsummary` missing-value rows into a single `dm` column on the label row, joining the per-group counts with `/`, then drops the missing rows.
  It is idempotent and returns the table unchanged when there is nothing to collapse, so it composes safely with the `tbl_format(collapse_missing = )` switch.
  The column header defaults to the language-dependent `labs$col_missing` option (`MD` in English, `DM` in French) and so requires `set_opts()`; `acro()` carries the matching entry, so `tbl_format()` expands the acronym in a footnote on its own.

- `acro()` adds the missing-data acronym to its built-in base, `MD` (missing data by group) in English configuration and `DM` (données manquantes par groupe) in French, matching the `col_missing()` column header.

- `get_opts()` returns the complete options object from the internal package store, restoring console inspection of the active options.

## Bug fixes

- `theme_gt(title_align)` reaches the title it names.
  The argument was passed to `gt::tab_options(heading.align = )`, then silently overridden by the `docx = FALSE` refinements, which restyled `cells_title()` and `cells_footnotes()` to `justify` in a single call.
  Every table therefore carried a justified title whatever the caller asked for, and a document wanting a centred one had to reach past `tbl_format()` with its own `gt::tab_style()`, which then aborted on the Word branch where `tbl_format()` returns a `flextable`.
  The refinement is split in two: the footnotes keep `justify`, the title takes `title_align`.
  Its default moves from `"left"` to `"justify"` so the rendered output is unchanged for every caller that never set it, and `tbl_format(title_align = "center")` now works on both branches, `theme_ft()` having always honoured the argument.
  The one place the new default is visible is `theme_gt(docx = TRUE)`, which skips that refinement and leaves the heading to `gt::tab_options()`: a title of more than one line is justified there where it used to be left-aligned.

- `tbl_format(title_align)` is a formal of its own rather than an argument travelling through `...`.
  The two themes it routes to default the alignment differently, `theme_gt()` justifying and `theme_ft()` left-aligning, so a table titled from a single source came out justified in HTML and left-aligned in Word.
  One value now drives both branches, defaulting to `"justify"`: the HTML output is unchanged, and a Word caption running to more than one line is justified where it used to be left-aligned.
  The themes keep their own defaults, which only a direct call to either of them now sees.

- `fct_str()` and `fct_keep()` no longer abort on a brace in the data.
  Both compose their entries with `glue()`, flatten them, then appended the final period with a second `glue()` call over that already-composed string.
  The second call re-ran interpolation on the data, so a value such as `drug {A}` was read as a glue component and errored with `Failed to evaluate glue component {A}`, a message naming nothing the caller could act on.
  Both functions take free text, `fct_keep()` splitting a delimited column and `fct_str()` counting factor levels, so the input is data rather than a literal.
  The period is now appended with `stringr::str_c()`, which concatenates without interpolating, and the returned value is the plain character string the documentation already promised rather than a `glue` object.

- `gtsum_format()` defines the `IRR` and `RR` estimators, the acronyms `gtsummary` writes for a Poisson-family fit with a log link and for a log-binomial one.
  Neither was declared, so the estimator footnote rendered the literal `IRR: NA; aIRR: adjusted NA`, and `estim_acro` could not rename them: the acronym recoding keyed on `Beta`, `exp(Beta)` and `HR` alone, and every other label fell through to its own value with no definition behind it.
  The `estim_acro` and `estim_label` slot sets gain `irr` and `rr`, defaulting to `incidence rate ratio` and `relative risk`.
  The `irr` slot follows the coefficient type read off the fitted model, not the estimand: a Poisson fit carrying no person-time offset estimates a ratio of expected counts rather than a rate ratio, which is what the two overrides are there to name.

- `easy_out()` writes SVGs that carry their font, so an exported figure no longer renders in a substitute for a reader who does not have that font installed.
  An SVG lands in a Quarto document as `<img src="data:image/svg+xml;base64,…">`, and an SVG referenced by `<img>` is an isolated document: the `@font-face` rules of the surrounding page never cross into it, so its `font-family` resolves against the reader's own fonts.
  The bundled Luciole regular and bold faces are now embedded into the file as `@font-face` blocks with a base64 WOFF2 `src:`, adding roughly 114 KB per figure.
  The new `web_fonts` argument overrides them, for a family the package does not bundle; build the blocks with `svglite::font_face(woff2 = <data URI>)` rather than `embed = TRUE`, which re-emits the face as uncompressed TTF at thirteen times the weight.
  A family with no bundled face posts a one-off hint instead of silently writing a figure that depends on the reader.

- `easy_out()` gives its device the font `set_opts()` resolved, so text that names no family renders in it.
  A character `shape` (`geom_point(shape = "x")`, a censor mark drawn as a glyph) is drawn with the device's default family and never with the theme's, so it fell through to whatever fontconfig returned, typically `Liberation Sans`.
  On a plot carrying no hebstr theme the gap covered every string, axis labels included.
  The same figure therefore rendered differently depending on whether it was read from a Quarto document, whose device carries the alias, or from the exported file.

- `col_missing()` no longer leaves a leading space in each `dm` cell under its default empty `prefix`.
  The template separated prefix from counts unconditionally, so a cell read `" 0/2"` instead of `"0/2"`.
  HTML collapses leading whitespace, so this surfaced in Word.
  A non-empty `prefix` keeps its separating space, `"n ="` still rendering `"n = 0/2"`.

- `add_ref_label()` localises its default `label`, which was the hardcoded English literal `"Reference"`.
  It now reads the `labs$reference` option, so a French session renders `Référence`.
  The canonical path was already correct, `gtsum_format()` passing the localised label down, so only a direct call was affected, which is what the function's own example shows.
  The default builds the options object without assigning it, so the call still works when `set_opts()` has never run.

- `easy_out(width = )` overrides the width a table carries from `tbl_format()`, applying it to the table and to the capture viewport alike.
  The argument was defaulted to `700` before the object's own `table.width` was read, which destroyed the "no value supplied" signal and made every explicit `width` inert on the standard pipeline, `tbl_format()` always baking a width in.
  Left `NULL`, a table declaring its own width still keeps it.

- `easy_out()` restarts its HTTP server when `getOption("easy_out.port")` changes, the singleton having been keyed on the output directory alone.
  A port set after the first export was ignored for the rest of the session, which is the value Positron's port forwarding needs and no public verb could reset.

- `easy_out()` percent-encodes the reserved characters of the served URL, path segment by path segment.
  A file whose name held a `#` or a `?` produced a URL the browser truncated at the fragment or the query, so a remote session opened a 404 on a file that was on disk; the other reserved characters (`&`, `+`, `;`) reached the server unescaped.

- `easy_out()` points a rejected `flextable` at `export = FALSE` when `hebstr.docx` is unset, rather than at the `export` default, which is `TRUE` in that state and reproduces the same error.

- `easy_out()` closes the SVG device when a grid grob fails to draw.
  The device was closed on the success path alone, so an error raised by `grid::grid.draw()` (a malformed `gpar`, an undefined viewport in a `boxGrob()` tree) left it current for the rest of the session and every later plot went silently into the half-written SVG.

- `svg_ink_box()` lets the rasterization error through instead of masking it.
  Its exit handler deleted a temporary file that `fs::file_temp()` never creates, so a failure before the raster was written surfaced as `[ENOENT] Failed to remove ...` in place of the real diagnostic.

- `tbl_format()` and `col_missing()` detect the missing rows with `%in%`, so a table whose body carries `NA` row types (for example the header row inserted by `add_label()`) no longer aborts with "missing value where TRUE/FALSE needed" when it holds no missing rows, as with `gtsummary::tbl_summary(missing = "no")`.

- `get_xlsx()` validates the sheet names with `rlang::is_named()`, so a list carrying an `NA` name raises the "fully named list" error instead of aborting on "missing value where TRUE/FALSE needed".

- `get_xlsx()` validates the `color` names the same way, so an unnamed or partially named list raises the "fully named list" error instead of being silently dropped by the per-sheet column filter.
  A name matching no column stays a no-op, that filter being what lets one palette serve sheets holding different columns.

- `get_xlsx()` keeps the header styling of a zero-row sheet.
  On an empty data frame `openxlsx2::wb_dims(select = "data")` collapses onto the header row, so the data font overwrote the header font and a `color =` highlight landed on the header instead of the data cells.

- `theme_bubble()` derives its grid colors with `%in%`, so `NA` axis colors (transparent, a valid ggplot2 color) propagate to the grid as a transparent color instead of aborting on "missing value where TRUE/FALSE needed".

- `check_fonts()` matches family names in full instead of on a word boundary, so a name shared with a wider family (`"Liberation"` against `"Liberation Sans"`) counts as missing rather than installed, since rendering it falls back to the system default.
  The device aliases `"sans"`, `"serif"` and `"mono"` always count as available.

- `easy_fct(.name = )` names the output column on the categorical branch as documented, instead of being ignored and silently overwriting the source column.

- `gt_qmd(id = )` reaches the table built from a `gtsummary` object, which was rendered with the `gt` default id.
  The argument still has no effect together with `top_n`, which delegates to `gt::gt_preview()`.

- `auto_exec()` returns `NULL` invisibly as documented, instead of the value of the closing `cli::cli_rule()`.

- `p_shortenr()` decides the `<` prefix on the raw p-value instead of the rounded one, so a value that only rounds up to the threshold is no longer flagged as below it (e.g. `0.0011` prints as `0.001`, not `<0.001`).

- `logit_lty()` errors early when the outcome is not a two-level factor, instead of silently returning an empty `$data`.

- `fct_other_str()` no longer emits a leading separator when no level falls below the minimum count.

- `easy_replace(replace = )` escapes the token before building the rule that collapses runs of it, so a token carrying regex metacharacters behaves as the literal string it is.
  With the documented `replace = "[X]"` the pattern read as a character class matching a bare `X`: the brackets were left orphaned in the output, consecutive tokens were not collapsed into a single marker, and unrelated standalone `X` characters were replaced.

- `add_label()` locates the insertion point with an exact name match, fixing a misplaced label when one variable name is a substring of another (e.g. `age` next to `age_group`).

- `merge_estim_ci(keep = TRUE)` returns the original numeric estimate and confidence-interval columns as documented, instead of their formatted character versions.

- `acro_extract()` matches an acronym that ends in a non-word character (e.g. `IC95%`), which the previous `\b`-bounded pattern could never match.

- `acro_extract()` returns a compound acronym whole (e.g. `n/N`) instead of separately matching its constituent parts, so a dictionary that holds both the parts (`n`, `N`) and the compound extracts the intended term.
  Matches within a string are now all returned, not just the first.

- `gtsum_format()` adds the events/observations column to a multivariable regression, not only to a univariate one: the column now appears whenever the table carries the `n_obs` count, so a `tbl_regression()` and a `tbl_uvregression()` render it alike.

- `gtsum_format()` blanks the estimate and confidence interval of a regression level carrying no event, on every table reporting an event count (Cox, logistic, Poisson).
  Such a level has no identifiable estimate (the coefficient diverges), and the fitted value rendered as a spurious `0.00` instead of being dropped.

- `add_note(rows = )` resolves its expression against the caller's environment, so a predicate citing a local variable works from inside a function, a loop, or a `map()`.
  The expression was captured without its environment and evaluated against the package namespace, whose scope chain ends at the global environment: a global variable resolved and a local one failed, reported as `rows` not evaluating to a logical vector rather than as an object not found.

- `easy_out(export = FALSE)` returns without clearing the variable context cached by `use_vars()`, as its documented "return without writing anything" implies.
  Under `options(hebstr.docx = TRUE)`, where the export is skipped by default, every call silently invalidated that cache while doing nothing else.

- `tbl_format()` renders the line break of a by-group header as a line break on the Word branch, where the `<br>` that `gtsum_format()` writes for the HTML branch used to print as literal text (`Total<br>(N=344)`).
  Each line keeps its own pair of bold markers, `flextable` reading markdown only from a pair wrapping the whole header.

- `tbl_format()` renders a univariate regression to Word instead of aborting on "non-numeric argument to binary operator".
  `gtsum_format()` rewrites the `stat_n` column that `tbl_uvregression()` creates as glued text, leaving behind the numeric formatter registered on it, which `flextable` applied where `gt` ignored it.

- `theme_ft()` keeps the row indentation of a `gtsummary` table, which `gtsummary` encodes as a left padding on the label column and the uniform horizontal padding of the theme flattened.
  Levels are indented under their variable again on the Word branch.

- `gtsum_format()` records the adjusted estimator definition for multivariable tables only, so the footnote of a univariate regression no longer defines an `aBeta` or `aOR` acronym the table never displays.

- `tbl_format()` no longer relays the "'big.mark' and 'decimal.mark' are both ','" warning that `flextable` triggers under a French locale when it numbers a footnote symbol.

- `gtsum_format()` localises the `beta` estimator definition on the decimal mark, like its `adj_label` companion, so a French table reads `aBeta : coefficient de régression ajusté` instead of mixing the French template with an English definition.
  `or` and `hr` keep their English wording, the terms in use in French biomedical writing; override them through `estim_label` if needed.

- `tbl_format()` runs `note_global` and the acronym definitions together into a single footnote on both output branches, the acronyms following the global note on the same line instead of being broken onto their own.

- `str_na_mv()` agrees its closing noun in the singular ("une donnée manquante").

## Minor improvements

- `get_vars_dict()` returns a third element, `json`, a copy of the summary meant for a JSON writer.
  It keeps the multi-valued cells as list columns marked with [base::I()], so `jsonlite::toJSON()` emits them as arrays rather than as the `" ; "` strings `data` carries, counts missing values as `0` rather than `NA`, and gives `p_miss` as a proportion rather than a formatted percentage.
  No argument gates it: the copy is a form of the dictionary, like the tibble and the widget, and a `json = TRUE` at a call site is now an error, the argument reaching [reactable::reactable()] through `...`.
  [easy_out()] writes it to a `.json` file beside the widget and the workbook, so reaching for a JSON writer by hand is only needed to send the dictionary somewhere other than disk.
- The columns of a `get_xlsx()` sheet are sized on what they hold at the size they are set in, header included.
  openxlsx2 measures a column in characters of the workbook's base font, so a sheet set in a smaller face came out wider than its text, by the ratio of the two sizes: a 27-character cell asked for 27 units of an 11 pt font while rendering at 8 pt, leaving about a quarter of the column empty on the right.
  The header was measured the same way and, carrying an autofilter button its own length does not account for, wrapped and clipped whenever it was the longest thing in its column: `n_miss` over an empty column came out as two lines of which only the first was visible.
  Each column now asks for the longest of its header and its values, each scaled by the size it is written at, plus the button allowance and a character of slack.
  `max_width` still caps the result.
- `get_xlsx(halign = )` also takes a named list, aligning the columns it names and leaving the others at the default `"center"`: `halign = list(variable = "left")`.
  A string still aligns the whole sheet, and an unnamed list is an error rather than a silent no-op.
  The alignment is posed on the column before the borders are broadcast, so it survives the per-column style spread that carries the number formats.
  The XLSX `easy_out()` writes beside a variable dictionary reads the same rule as the widget, so its `variable`, `label` and `levels` columns are left-aligned and the rest centred.
- The `border_color` default of a `get_xlsx()` sheet lightens from `"#999999"` to `"#C5C5C5"`, the grid reading as a rule between cells rather than as a second layer of content.
  `wb_add_custom()` keeps its own default.
- The cells of the `get_vars_dict()` widget are centred, except those of `variable`, `label` and `levels`, which stay left-aligned.
  Alignment used to be left to reactable, which aligns on the storage type: the counts came out right-aligned, the codes left-aligned, and no column lined up with its header.
  The three text columns are the ones a reader scans down rather than compares, so they keep a common left edge.
- The columns of the `get_vars_dict()` widget are sized on what they hold rather than on a fixed width per column name.
  Each column asks for the width of its longest value, header included, counted at 0.47 em a character plus the cell padding and bounded to between 50 and 250 pixels, so that one verbose label neither starves its neighbours nor pushes the table into a horizontal scroll.
  The em figure is measured, not guessed: a canvas at the widget's own font size gives between 0.41 and 0.48 em a character on the strings long enough to decide a width.
  Reading it from `font_size` rather than from a constant keeps the widths honest when that argument moves, `rem`, `em`, `px`, `pt` and `%` included.
  The widths are minimums in a flexbox layout, so the columns still share whatever room the container has left.
  Selecting a subset through `cols` no longer leaves the columns that had no hard-coded entry at the generic default.
  The `font_size` default itself moves from `"0.7rem"` to `"0.65rem"`, and the widths follow it, being read from that argument rather than from a constant.
- `easy_out(crop = TRUE)` finds the modal background of the raster through `tabulate()` rather than `table()`, which coerced every pixel to character.
  About 35 times faster on the ink-box measurement, roughly eight tenths of a second off every cropped grob export.
- `docx_page_width()` reads the section geometry of a `.docx` or `.dotx` template and returns the width its body text can use, in inches, landscape included.
  Feeding it to `set_opts(page_width = )` derives the reference from the `reference-doc` in use instead of copying a number that drifts the day its margins change.
  Its `path` is optional: left out, the template of the document being rendered is looked up in its YAML front matter, then in the Quarto extension providing the Word format.
  The two sources are complementary, a document declaring `format: docx` carrying its template in the front matter, one declaring `format: <ext>-docx` carrying it in the extension manifest, out of reach of the front matter.
  The extension is searched in the nearest `_extensions` directory, from the document upwards; several extensions contributing a `docx` format aborts, none being more legitimate than the others.
  A template declaring no page geometry, as the `reference.docx` Pandoc ships does, is now reported as such instead of failing on a subscript.
- `set_opts()` carries a `page_width` key, in inches, which `tbl_format(page_width = )` now defaults to.
  The usable text width is a property of the rendered Word document, not of each table: declaring it once makes every table convert its pixel width against the `reference-doc` actually in use, instead of the US Letter assumption repeated at each call site.
  It is unset by default, and `tbl_format()` then runs the `docx_page_width()` lookup itself, on the Word branch only: a document rendering through a Quarto extension states the width nowhere.
  Set the key when the lookup cannot reach the template, which a warning reports.
  Falling back to `6.5` stays silent when no template is declared at all, that being the documented default rather than a failed measurement.
- `gtsum_format()` labels the regression count column `n/N` by default (events over observations), and `N` when the model carries no events, replacing the former `Events/Obs` and `Obs` headers.
  Override with `label_n`.
- `acro()` adds the `n/N` acronym (events over total observations) to its built-in English and French dictionaries, so the `n/N` count column that `gtsum_format()` emits expands to a full footnote definition.
- `check_fonts()` tests the session font registry (`systemfonts::registry_fonts()`) alongside the system font list, so a family registered from bundled font files with `systemfonts::register_font()` counts as available instead of falling back to the default.
- `easy_descr()` prints its variable classification as a single per-variable tibble (name, storage type, statistical group) instead of grouped variable lists, with storage-type codes aligned on those of `get_vars_dict()`.
- `easy_out()` renders plot SVG with the `svglite` device instead of `grDevices::svg` (via `htmltools::capturePlot`), producing cleaner, more portable SVG and dropping the `htmltools` and `grDevices` dependencies.
- `easy_out()` lowers its default raster resolution `px` from 2000 to 1200, producing smaller PNG files by default; pass `px = 2000` to restore the previous resolution.
- `get_xlsx()` builds large sheets in near-linear time instead of quadratic (17,200 rows by 5 columns: 0.6 second instead of 16).
  Every cell of a column carries the same border and the same number format, so the border is now resolved on the first two rows and the resolved style broadcast down each column, rather than passed to `openxlsx2::wb_add_border()` over the whole range.
  The resulting styles are unchanged, cell for cell.
