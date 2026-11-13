make_cache <- function() {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("a", file.path(d, "1a2b3c4d-wfo.zip"))
  writeLines("bb", file.path(d, "ffffffff-ppg.csv"))
  writeLines("x", file.path(d, "file1234.part"))
  writeLines("mine", file.path(d, "notes.txt"))
  d
}

test_that("ts_show_ref_cache() lists our files only", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- make_cache()
  expect_message(ts_show_ref_cache(d), "Downloaded files")
  x <- suppressMessages(ts_show_ref_cache(d))
  expect_identical(x$file, c("file1234.part", "ppg.csv", "wfo.zip"))
  expect_named(x, c("file", "size", "modified", "path"))
  expect_false(any(grepl("notes.txt", x$path)))
})

test_that("ts_show_ref_cache() handles empty and missing folders", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- withr::local_tempdir()
  expect_message(ts_show_ref_cache(d), "No downloaded files")
  expect_identical(nrow(suppressMessages(ts_show_ref_cache(d))), 0L)
  nope <- file.path(d, "nope")
  expect_message(ts_show_ref_cache(nope), "No downloaded")
  expect_identical(nrow(suppressMessages(ts_show_ref_cache(nope))), 0L)
  expect_false(dir.exists(file.path(d, "nope")))
})

test_that("ts_clear_ref_cache() deletes ours and keeps other files", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- make_cache()
  expect_message(ts_clear_ref_cache(d, ask = FALSE), "Deleted 3")
  expect_identical(list.files(d), "notes.txt")
  expect_message(ts_clear_ref_cache(d, ask = FALSE), "Nothing to delete")
  del <- suppressMessages(ts_clear_ref_cache(make_cache(), ask = FALSE))
  expect_length(del, 3)
})

test_that("ts_clear_ref_cache() older_than keeps recent files", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- make_cache()
  Sys.setFileTime(file.path(d, "1a2b3c4d-wfo.zip"), Sys.time() - 10 * 86400)
  del <- suppressMessages(ts_clear_ref_cache(d, older_than = 5, ask = FALSE))
  expect_identical(basename(del), "1a2b3c4d-wfo.zip")
  expect_length(list.files(d), 3)
})

test_that("ts_clear_ref_cache() asks only when told to, and respects 'no'", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- make_cache()
  testthat::local_mocked_bindings(askYesNo = function(...) FALSE,
    .package = "utils")
  expect_message(ts_clear_ref_cache(d, ask = TRUE), "Nothing deleted")
  expect_length(list.files(d), 4)
  expect_false(interactive()) # default is no prompt when not interactive
  suppressMessages(ts_clear_ref_cache(d))
  expect_identical(list.files(d), "notes.txt")
})

test_that("cache functions use the option", {
  withr::local_options(taxastand.verbosity = "inform")
  d <- make_cache()
  withr::local_options(taxastand.cache_dir = d)
  expect_identical(nrow(suppressMessages(ts_show_ref_cache())), 3L)
})
