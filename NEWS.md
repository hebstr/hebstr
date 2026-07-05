# hebstr 0.0.0.9000

## Breaking changes

Package state no longer touches the user's workspace. Options and the variable classification cache now live in an internal package store instead of the global environment, so the package complies with the CRAN policy against writing to `.GlobalEnv`.

* `set_opts()` no longer creates an `opts` object in the global environment; it stores the options in the internal package store. Read the whole object with the new `get_opts()`, or a single validated key with `check_opts()`.
* `easy_view()` loses its `assign` argument (and the `name` argument, which only served to name the assigned widget). It returns the widget; assign it yourself if you need a bound name.
* `clear_vars()` loses its `env` argument and is now called with no arguments; it always clears the internal cache set by `use_vars()`.

## New features

* `get_opts()` returns the complete options object from the internal package store, restoring console inspection of the active options.
