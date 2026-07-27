# hebstr 0.0.0.9000

## Breaking changes

Package state no longer touches the user's workspace.
Options and the variable classification cache now live in an internal package store instead of the global environment, so the package complies with the CRAN policy against writing to `.GlobalEnv`.

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
- `easy_view()` loses its `assign` argument (and the `name` argument, which only served to name the assigned widget).
  It returns the widget; assign it yourself if you need a bound name.
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
  Exporting a `flextable` remains out of scope, and forcing one still errors, naming `hebstr.docx` and the `export` argument.
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
- `tbl_format()` renders `note_global` and the acronym definitions as two footnotes on the gt branch, as it already did on the Word branch, instead of running them together into one paragraph.
- `str_na_mv()` agrees its closing noun in the singular ("une donnée manquante").

## Minor improvements

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
- `easy_descr()` prints its variable classification as a single per-variable tibble (name, storage type, statistical group) instead of grouped variable lists, with storage-type codes aligned on those of `easy_view()`.
- `easy_out()` renders plot SVG with the `svglite` device instead of `grDevices::svg` (via `htmltools::capturePlot`), producing cleaner, more portable SVG and dropping the `htmltools` and `grDevices` dependencies.
- `easy_out()` lowers its default raster resolution `px` from 2000 to 1200, producing smaller PNG files by default; pass `px = 2000` to restore the previous resolution.
- `get_xlsx()` builds large sheets in near-linear time instead of quadratic (17,200 rows by 5 columns: 0.6 second instead of 16).
  Every cell of a column carries the same border and the same number format, so the border is now resolved on the first two rows and the resolved style broadcast down each column, rather than passed to `openxlsx2::wb_add_border()` over the whole range.
  The resulting styles are unchanged, cell for cell.
