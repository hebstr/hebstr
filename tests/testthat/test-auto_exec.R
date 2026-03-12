test_that("auto_exec() errors when directory does not exist", {
  expect_error(
    auto_exec(dir = "nonexistent_dir_abc123"),
    class = "rlang_error"
  )
})

test_that("auto_exec() errors when no matching files found", {
  dir <- withr::local_tempdir()
  writeLines("x <- 1", file.path(dir, "_hidden.R"))

  expect_error(
    auto_exec(dir = dir, except_starts_with = "_", ext = ".R"),
    class = "rlang_error"
  )
})

test_that("auto_exec() errors when only non-.R files exist", {
  dir <- withr::local_tempdir()
  writeLines("hello", file.path(dir, "readme.txt"))

  expect_error(
    auto_exec(dir = dir),
    class = "rlang_error"
  )
})

test_that("auto_exec() sources .R files in directory", {
  dir <- withr::local_tempdir()
  writeLines("test_auto_exec_val_1 <- 42", file.path(dir, "script1.R"))
  writeLines("test_auto_exec_val_2 <- 99", file.path(dir, "script2.R"))
  withr::defer(rm(test_auto_exec_val_1, test_auto_exec_val_2, envir = globalenv()))

  auto_exec(dir = dir)

  expect_equal(get("test_auto_exec_val_1", envir = globalenv()), 42)
  expect_equal(get("test_auto_exec_val_2", envir = globalenv()), 99)
})

test_that("auto_exec() excludes files starting with prefix", {
  dir <- withr::local_tempdir()
  writeLines("test_auto_exec_included <- TRUE", file.path(dir, "run.R"))
  writeLines("test_auto_exec_excluded <- TRUE", file.path(dir, "_skip.R"))
  withr::defer(rm(test_auto_exec_included, envir = globalenv()))

  auto_exec(dir = dir, except_starts_with = "_")

  expect_true(get("test_auto_exec_included", envir = globalenv()))
  expect_false(exists("test_auto_exec_excluded", envir = globalenv()))
})

test_that("auto_exec() respects custom prefix", {
  dir <- withr::local_tempdir()
  writeLines("test_auto_exec_ok <- TRUE", file.path(dir, "main.R"))
  writeLines("test_auto_exec_draft <- TRUE", file.path(dir, "draft_test.R"))
  withr::defer(rm(test_auto_exec_ok, envir = globalenv()))

  auto_exec(dir = dir, except_starts_with = "draft")

  expect_true(get("test_auto_exec_ok", envir = globalenv()))
  expect_false(exists("test_auto_exec_draft", envir = globalenv()))
})

test_that("auto_exec() respects custom ext", {
  dir <- withr::local_tempdir()
  writeLines("test_auto_exec_r <- TRUE", file.path(dir, "code.R"))
  writeLines("test_auto_exec_txt <- TRUE", file.path(dir, "code.txt"))
  withr::defer(rm(test_auto_exec_r, envir = globalenv()))

  auto_exec(dir = dir, ext = ".R")

  expect_true(get("test_auto_exec_r", envir = globalenv()))
  expect_false(exists("test_auto_exec_txt", envir = globalenv()))
})

test_that("auto_exec() only sources files matching ext", {
  dir <- withr::local_tempdir()
  writeLines("test_auto_exec_rmd <- TRUE", file.path(dir, "report.Rmd"))
  writeLines("test_auto_exec_r2 <- TRUE", file.path(dir, "analysis.R"))
  withr::defer(rm(test_auto_exec_r2, envir = globalenv()))

  auto_exec(dir = dir, ext = ".R")

  expect_true(get("test_auto_exec_r2", envir = globalenv()))
  expect_false(exists("test_auto_exec_rmd", envir = globalenv()))
})
