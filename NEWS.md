# hebstr 0.0.0.9000

## Breaking changes

Package state no longer touches the user's workspace. Options and the variable classification cache now live in an internal package store instead of the global environment, so the package complies with the CRAN policy against writing to `.GlobalEnv`.

* `set_opts()` no longer creates an `opts` object in the global environment; it stores the options in the internal package store. Read the whole object with the new `get_opts()`, or a single validated key with `check_opts()`.
* `easy_view()` loses its `assign` argument (and the `name` argument, which only served to name the assigned widget). It returns the widget; assign it yourself if you need a bound name.
* `clear_vars()` loses its `env` argument and is now called with no arguments; it always clears the internal cache set by `use_vars()`.
* The default text and numeric font is now the portable `"sans"` family instead of Luciole, so output no longer depends on a non-standard font being installed. Opt into Luciole with `set_opts(font = "luciole")`.
* The minimum required R version is now 4.5.0 (was 4.1.0), because examples and helpers rely on the `penguins` dataset shipped with base R since 4.5.0.
* `str_fig()` renames its `qmd` argument to `render` and defaults it to `getOption("str_fig.render", TRUE)` (was `FALSE`), so the output targets a Quarto context by default. Pass `render = FALSE`, or set `options(str_fig.render = FALSE)`, for the inline font-size formatting.

## New features

* `get_opts()` returns the complete options object from the internal package store, restoring console inspection of the active options.

## Bug fixes

* `p_shortenr()` decides the `<` prefix on the raw p-value instead of the rounded one, so a value that only rounds up to the threshold is no longer flagged as below it (e.g. `0.0011` prints as `0.001`, not `<0.001`).
* `logit_lty()` errors early when the outcome is not a two-level factor, instead of silently returning an empty `$data`.
* `fct_other_str()` no longer emits a leading separator when no level falls below the minimum count.
* `add_label()` locates the insertion point with an exact name match, fixing a misplaced label when one variable name is a substring of another (e.g. `age` next to `age_group`).
* `merge_estim_ci(keep = TRUE)` returns the original numeric estimate and confidence-interval columns as documented, instead of their formatted character versions.
* `acro_extract()` matches an acronym that ends in a non-word character (e.g. `IC95%`), which the previous `\b`-bounded pattern could never match.

## Minor improvements

* `easy_out()` renders plot SVG with the `svglite` device instead of `grDevices::svg` (via `htmltools::capturePlot`), producing cleaner, more portable SVG and dropping the `htmltools` and `grDevices` dependencies.
* `easy_out()` lowers its default raster resolution `px` from 2000 to 1200, producing smaller PNG files by default; pass `px = 2000` to restore the previous resolution.
