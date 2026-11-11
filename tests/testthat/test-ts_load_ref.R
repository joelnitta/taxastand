zip_fixture <- test_path("fixtures", "mini-dwca.zip")

test_that("a Darwin Core Archive is read and formatted", {
  res <- ts_load_ref(zip_fixture, version = "test", quiet = TRUE)
  expect_equal(res$taxonID, c("3", "4", "7"))
  expect_equal(res$scientificName[1:2], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd."
  ))
  expect_equal(unique(res$taxonomicStatus), c("accepted", "synonym"))
  src <- attr(res, "ts_source")
  expect_equal(src$source, zip_fixture)
  expect_equal(src$version, "test")
  expect_true(!is.null(src$retrieved))
})

test_that("taxon subsets before formatting", {
  res <- ts_load_ref(
    zip_fixture, taxon = "Polypodiaceae", quiet = TRUE
  )
  expect_equal(res$taxonID, c("3", "4"))
})

test_that("csv and tsv files are read", {
  x <- data.frame(
    taxonID = c("1", "2"), acceptedNameUsageID = c("1", "1"),
    taxonomicStatus = c("accepted", "synonym"),
    scientificName = c("Foo bar L.", "Foo baz Willd.")
  )
  f_csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(x, f_csv, row.names = FALSE)
  f_tsv <- withr::local_tempfile(fileext = ".tsv")
  write.table(x, f_tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  expect_equal(
    ts_load_ref(f_csv, quiet = TRUE)$scientificName, x$scientificName
  )
  expect_equal(
    ts_load_ref(f_tsv, quiet = TRUE)$scientificName, x$scientificName
  )
})

test_that("downloads are cached and can be refreshed (no internet used)", {
  calls <- 0
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    calls <<- calls + 1
    file.copy(zip_fixture, dest, overwrite = TRUE)
    invisible(dest)
  })
  cache <- withr::local_tempdir()
  url <- "https://example.org/data/ref.zip"
  res <- ts_load_ref(url, cache_dir = cache, quiet = TRUE)
  expect_equal(nrow(res), 3)
  expect_equal(attr(res, "ts_source")$source, url)
  expect_equal(length(list.files(cache)), 1)
  expect_equal(calls, 1)
  ts_load_ref(url, cache_dir = cache, quiet = TRUE)
  expect_equal(calls, 1)
  ts_load_ref(url, cache_dir = cache, refresh = TRUE, quiet = TRUE)
  expect_equal(calls, 2)
  # no partial files left
  expect_false(any(grepl("\\.part$", list.files(cache))))
})

test_that("the default cache is in tempdir() and the option is used", {
  expect_true(startsWith(
    normalizePath(.ts_cache_dir(), mustWork = FALSE),
    normalizePath(tempdir(), mustWork = FALSE)
  ))
  d <- withr::local_tempdir()
  withr::local_options(taxastand.cache_dir = file.path(d, "opt"))
  expect_equal(.ts_cache_dir(), file.path(d, "opt"))
  expect_true(dir.exists(file.path(d, "opt")))
})

test_that("failed downloads give an informative error and leave nothing", {
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    writeLines("partial", dest)
    stop("cannot open URL")
  })
  cache <- withr::local_tempdir()
  expect_error(
    ts_load_ref("https://example.org/x.csv", cache_dir = cache, quiet = TRUE),
    "Could not download https://example.org/x.csv: cannot open URL"
  )
  expect_length(list.files(cache), 0)
})

test_that("input checks work", {
  expect_error(ts_load_ref("http://example.org/x.csv"), "HTTPS")
  expect_error(ts_load_ref("no/such/file.csv"), "not an existing file")
  expect_error(ts_load_ref(1))
  expect_error(ts_load_ref(zip_fixture, refresh = NA, quiet = TRUE))
})

test_that("the result works with ts_resolve_names()", {
  ref <- ts_load_ref(zip_fixture, quiet = TRUE)
  res <- ts_resolve_names("Polypodium rotundatum Willd.", ref)
  expect_equal(res$resolved_name, "Polypodium vulgare L.")
})

test_that("zip files are recognised by content, not by name", {
  f <- withr::local_tempfile(fileext = ".dat")
  file.copy(zip_fixture, f)
  expect_true(.ts_is_zip(f))
  expect_equal(nrow(ts_load_ref(f, quiet = TRUE)), 3)
  txt <- withr::local_tempfile(fileext = ".zip")
  writeLines("a,b\n1,2", txt)
  expect_false(.ts_is_zip(txt))
})
