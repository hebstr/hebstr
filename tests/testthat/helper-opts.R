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

local_opts <- \(..., .env = parent.frame()) {
  local_hebstr("opts", .env = .env)
  local_vars_context(.env = .env)
  local_estim_channel(.env = .env)
  set_opts(...)

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
