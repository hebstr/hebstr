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

* `easy_out()` accepts a grid grob (for example a Gmisc flowchart built from `boxGrob()`/`connectGrob()`), exporting it to SVG and PNG through the same pipeline as ggplot objects.
* `get_opts()` returns the complete options object from the internal package store, restoring console inspection of the active options.

## Bug fixes

* `p_shortenr()` decides the `<` prefix on the raw p-value instead of the rounded one, so a value that only rounds up to the threshold is no longer flagged as below it (e.g. `0.0011` prints as `0.001`, not `<0.001`).
* `logit_lty()` errors early when the outcome is not a two-level factor, instead of silently returning an empty `$data`.
* `fct_other_str()` no longer emits a leading separator when no level falls below the minimum count.
* `add_label()` locates the insertion point with an exact name match, fixing a misplaced label when one variable name is a substring of another (e.g. `age` next to `age_group`).
* `merge_estim_ci(keep = TRUE)` returns the original numeric estimate and confidence-interval columns as documented, instead of their formatted character versions.
* `acro_extract()` matches an acronym that ends in a non-word character (e.g. `IC95%`), which the previous `\b`-bounded pattern could never match.
* `acro_extract()` returns a compound acronym whole (e.g. `n/N`) instead of separately matching its constituent parts, so a dictionary that holds both the parts (`n`, `N`) and the compound extracts the intended term. Matches within a string are now all returned, not just the first.
* `gtsum_format()` adds the events/observations column to a multivariable regression, not only to a univariate one: the column now appears whenever the table carries the `n_obs` count, so a `tbl_regression()` and a `tbl_uvregression()` render it alike.

## Minor improvements

* `gtsum_format()` labels the regression count column `n/N` by default (events over observations), and `N` when the model carries no events, replacing the former `Events/Obs` and `Obs` headers. Override with `label_n`.
* `acro()` adds the `n/N` acronym (events over total observations) to its built-in English and French dictionaries, so the `n/N` count column that `gtsum_format()` emits expands to a full footnote definition.
* `easy_descr()` prints its variable classification as a single per-variable tibble (name, storage type, statistical group) instead of grouped variable lists, with storage-type codes aligned on those of `easy_view()`.
* `easy_out()` renders plot SVG with the `svglite` device instead of `grDevices::svg` (via `htmltools::capturePlot`), producing cleaner, more portable SVG and dropping the `htmltools` and `grDevices` dependencies.
* `easy_out()` lowers its default raster resolution `px` from 2000 to 1200, producing smaller PNG files by default; pass `px = 2000` to restore the previous resolution.
