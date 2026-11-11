zip_fixture <- test_path("fixtures", "mini-dwca.zip")

test_that("a Darwin Core Archive is read and formatted", {
  res <- ts_load_ref(file = zip_fixture, version = "test", quiet = TRUE)
  expect_equal(res$taxonID, c("3", "4", "7"))
  expect_equal(res$scientificName[1:2], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd."
  ))
  expect_equal(unique(res$taxonomicStatus), c("accepted", "synonym"))
  src <- attr(res, "ts_source")
  expect_equal(src$file, zip_fixture)
  expect_equal(src$version, "test")
  expect_true(!is.null(src$retrieved))
})

test_that("taxon subsets before formatting", {
  res <- ts_load_ref(
    file = zip_fixture, taxon = "Polypodiaceae", quiet = TRUE
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
    ts_load_ref(file = f_csv, quiet = TRUE)$scientificName, x$scientificName
  )
  expect_equal(
    ts_load_ref(file = f_tsv, quiet = TRUE)$scientificName, x$scientificName
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
  res <- ts_load_ref(url = url, cache_dir = cache, quiet = TRUE)
  expect_equal(nrow(res), 3)
  expect_equal(attr(res, "ts_source")$url, url)
  expect_equal(length(list.files(cache)), 1)
  expect_equal(calls, 1)
  ts_load_ref(url = url, cache_dir = cache, quiet = TRUE)
  expect_equal(calls, 1)
  ts_load_ref(url = url, cache_dir = cache, refresh = TRUE, quiet = TRUE)
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
    ts_load_ref(url = "https://example.org/x.csv", cache_dir = cache, quiet = TRUE),
    "Could not download https://example.org/x.csv: cannot open URL"
  )
  expect_length(list.files(cache), 0)
})

test_that("input checks work", {
  expect_error(ts_load_ref(url = "http://example.org/x.csv"), "HTTPS")
  expect_error(ts_load_ref(file = "no/such/file.csv"), "not an existing file")
  expect_error(ts_load_ref(file = 1))
  expect_error(ts_load_ref(file = zip_fixture, refresh = NA, quiet = TRUE))
})

test_that("the result works with ts_resolve_names()", {
  ref <- ts_load_ref(file = zip_fixture, quiet = TRUE)
  res <- ts_resolve_names("Polypodium rotundatum Willd.", ref)
  expect_equal(res$resolved_name, "Polypodium vulgare L.")
})

test_that("zip files are recognised by content, not by name", {
  f <- withr::local_tempfile(fileext = ".dat")
  file.copy(zip_fixture, f)
  expect_true(.ts_is_zip(f))
  expect_equal(nrow(ts_load_ref(file = f, quiet = TRUE)), 3)
  txt <- withr::local_tempfile(fileext = ".zip")
  writeLines("a,b\n1,2", txt)
  expect_false(.ts_is_zip(txt))
})

test_that("exactly one of source, url and file is needed", {
  expect_error(ts_load_ref(), "exactly one")
  expect_error(ts_load_ref(source = "wfo", file = zip_fixture), "exactly one")
  expect_error(ts_load_ref(source = "nope", quiet = TRUE), "Unknown `source`")
})

zenodo_json <- function(id, version) {
  sprintf(
    paste0(
      '{"created": "2026-06-22", "id": %s, "conceptrecid": "7460141", ',
      '"metadata": {"title": "WFO", "version": "%s"}, "files": []}'
    ),
    id, version
  )
}

test_that("wfo: latest and specific versions are resolved and recorded", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) {
      if (grepl("versions/latest", link)) {
        zenodo_json(222, "2026-06")
      } else {
        expect_match(link, "records/222/versions")
        paste0('{"hits": {"hits": [', zenodo_json(222, "2026-06"), ", ",
          zenodo_json(111, "2025-12"), "]}}")
      }
    }
  )
  expect_equal(
    .ts_resolve_wfo(NULL)$url,
    "https://zenodo.org/records/222/files/_DwC_backbone_R.zip?download=1"
  )
  expect_equal(.ts_resolve_wfo("2025-12")$version, "2025-12")
  expect_match(.ts_resolve_wfo("2025-12")$url, "records/111/")
  expect_error(.ts_resolve_wfo("1999-01"), "not found")
})

test_that("wfo: tab-separated file with quotes is read and formatted", {
  urls <- character()
  local_mocked_bindings(
    .ts_fetch_text = function(link) zenodo_json(222, "2026-06"),
    .ts_fetch = function(url, dest) {
      urls <<- c(urls, url)
      file.copy(test_path("fixtures", "mini-wfo.zip"), dest, overwrite = TRUE)
      invisible(dest)
    }
  )
  cache <- withr::local_tempdir()
  expect_message(
    res <- ts_load_ref(source = "wfo", cache_dir = cache),
    "License: CC0"
  )
  expect_match(urls, "records/222/")
  # genus and family rows are dropped; the stray quote in the remarks did not
  # stop the reading; wrapping quotes are removed; the author is added
  expect_equal(res$taxonID, c("wfo-3", "wfo-4", "wfo-5", "wfo-6"))
  expect_equal(res$scientificName[1:2], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd."
  ))
  # only the columns that are needed are read
  expect_false("taxonRemarks" %in% colnames(res))
  expect_true("family" %in% colnames(res))
  # unchecked names are kept
  expect_equal(res$taxonomicStatus[4], "unchecked")
  src <- attr(res, "ts_source")
  expect_equal(src$source, "wfo")
  expect_equal(src$version, "2026-06")
  expect_equal(src$license, "CC0 1.0")
  expect_match(src$citation, "zenodo.7460141")

  sub <- ts_load_ref(
    source = "wfo", cache_dir = cache, taxon = "Polypodiaceae", quiet = TRUE
  )
  expect_equal(sub$taxonID, c("wfo-3", "wfo-4"))
})

test_that("ppg: the latest release is found and the csv is read", {
  fetched <- character()
  local_mocked_bindings(
    .ts_fetch_text = function(link) '{"url": "x", "tag_name": "v2.1.0", "name": "v"}',
    .ts_fetch = function(url, dest) {
      fetched <<- url
      file.copy(test_path("fixtures", "mini-ppg.csv"), dest, overwrite = TRUE)
      invisible(dest)
    }
  )
  res <- ts_load_ref(
    source = "ppg", cache_dir = withr::local_tempdir(), quiet = TRUE
  )
  expect_equal(
    fetched,
    "https://raw.githubusercontent.com/pteridogroup/ppg/v2.1.0/data/ppg.csv"
  )
  expect_equal(res$taxonID, c("wfo-3", "wfo-4", "wfo-5"))
  expect_equal(attr(res, "ts_source")$version, "v2.1.0")
  # a table with only parentNameUsageID is subset by walking down the parents
  sub <- ts_load_ref(
    source = "ppg", version = "v2.0.0", cache_dir = withr::local_tempdir(),
    taxon = "Polypodiaceae", quiet = TRUE
  )
  expect_equal(sub$taxonID, c("wfo-3", "wfo-4"))
  expect_equal(attr(sub, "ts_source")$version, "v2.0.0")
})

test_that("failure to reach a source gives an informative error", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("cannot open URL", call. = FALSE)
  )
  expect_error(ts_load_ref(source = "ppg", quiet = TRUE), "cannot open URL")
})

test_that("ts_reference_sources() lists the sources", {
  x <- ts_reference_sources()
  expect_equal(x$source, c("wfo", "ppg"))
  expect_true(all(nzchar(x$license)) && all(nzchar(x$citation)))
})
