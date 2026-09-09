.local_binding <- \(name, envir, .env = parent.frame()) {
  had <- exists(name, envir = envir, inherits = FALSE)
  old <- if (had) get(name, envir = envir, inherits = FALSE)

  withr::defer(
    if (had) {
      assign(name, old, envir = envir)
    } else if (exists(name, envir = envir, inherits = FALSE)) {
      rm(list = name, envir = envir)
    },
    envir = .env
  )

  invisible(NULL)
}

local_hebstr <- \(name, value, .env = parent.frame()) {
  .local_binding(name, .hebstr, .env = .env)

  if (missing(value)) {
    if (exists(name, envir = .hebstr, inherits = FALSE)) {
      rm(list = name, envir = .hebstr)
    }
  } else {
    assign(name, value, envir = .hebstr)
  }

  invisible(NULL)
}

# A server handle is a resource, not a value: once stopped there is nothing to
# put back, so the invariant is "no server before, no server after" rather than
# the restore that .local_binding() gives the other .hebstr bindings.
local_server <- \(.env = parent.frame()) {
  browse_stop()
  withr::defer(browse_stop(), envir = .env)

  invisible(NULL)
}

# set_opts() writes reactable.theme, which lives in the session options and not
# in .hebstr, so the restore of the three bindings above would leave it behind
# for the next file and make the suite order-dependent.
local_opts <- \(..., .env = parent.frame()) {
  local_hebstr("opts", .env = .env)
  local_vars_context(.env = .env)
  local_estim_channel(.env = .env)
  withr::local_options(
    reactable.theme = getOption("reactable.theme"),
    .local_envir = .env
  )
  set_opts(...)

  invisible(NULL)
}

# the render target lives in knitr's own settings, so a test drives
# is_html_output() itself instead of mocking it
local_pandoc_to <- \(to, .env = parent.frame()) {
  old <- knitr::opts_knit$get("rmarkdown.pandoc.to")

  withr::defer(knitr::opts_knit$set(rmarkdown.pandoc.to = old), envir = .env)

  knitr::opts_knit$set(rmarkdown.pandoc.to = to)

  invisible(NULL)
}

local_vars_context <- \(.env = parent.frame()) {
  .local_binding(".vars_context", .hebstr, .env = .env)

  invisible(NULL)
}

rm_vars_context <- \() {
  if (exists(".vars_context", envir = .hebstr, inherits = FALSE)) {
    rm(list = ".vars_context", envir = .hebstr)
  }

  invisible(NULL)
}

local_estim_channel <- \(.env = parent.frame()) {
  old <- as.list(.estim_channel, all.names = TRUE)

  withr::defer(
    {
      rm(
        list = ls(envir = .estim_channel, all.names = TRUE),
        envir = .estim_channel
      )
      list2env(old, envir = .estim_channel)
    },
    envir = .env
  )

  invisible(NULL)
}

rm_estim_channel <- \() {
  rm(
    list = ls(envir = .estim_channel, all.names = TRUE),
    envir = .estim_channel
  )

  invisible(NULL)
}
