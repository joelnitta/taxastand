zip_fixture <- test_path("fixtures", "mini-dwca.zip")

test_that("a Darwin Core Archive is read and formatted", {
  res <- ts_load_ref(file = zip_fixture, version = "test")
  expect_identical(res$taxonID, c("3", "4", "7"))
  expect_identical(res$scientificName[1:2], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd."
  ))
  expect_identical(unique(res$taxonomicStatus), c("accepted", "synonym"))
  src <- attr(res, "ts_source")
  expect_identical(src$file, zip_fixture)
  expect_identical(src$version, "test")
  expect_false(is.null(src$retrieved))
})

test_that("taxon subsets before formatting", {
  res <- ts_load_ref(
    file = zip_fixture, taxon = "Polypodiaceae"
  )
  expect_identical(res$taxonID, c("3", "4"))
})

test_that("csv and tsv files are read", {
  x <- data.frame(
    taxonID = c("1", "2"), acceptedNameUsageID = c("1", "1"),
    taxonomicStatus = c("accepted", "synonym"),
    scientificName = c("Foo bar L.", "Foo baz Willd."),
    stringsAsFactors = FALSE
  )
  f_csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(x, f_csv, row.names = FALSE)
  f_tsv <- withr::local_tempfile(fileext = ".tsv")
  write.table(x, f_tsv, sep = "\t", row.names = FALSE, quote = FALSE)
  expect_identical(
    ts_load_ref(file = f_csv)$scientificName, x$scientificName
  )
  expect_identical(
    ts_load_ref(file = f_tsv)$scientificName, x$scientificName
  )
})

test_that("downloads are cached and can be refreshed (no internet used)", {
  seen <- new.env()
  seen$calls <- 0
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    seen$calls <- seen$calls + 1
    file.copy(zip_fixture, dest, overwrite = TRUE)
    invisible(dest)
  })
  cache <- withr::local_tempdir()
  url <- "https://example.org/data/ref.zip"
  res <- ts_load_ref(url = url, cache_dir = cache)
  expect_identical(nrow(res), 3L)
  expect_identical(attr(res, "ts_source")$url, url)
  expect_length(list.files(cache), 1)
  expect_identical(seen$calls, 1)
  ts_load_ref(url = url, cache_dir = cache)
  expect_identical(seen$calls, 1)
  ts_load_ref(url = url, cache_dir = cache, refresh = TRUE)
  expect_identical(seen$calls, 2)
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
  expect_identical(.ts_cache_dir(), file.path(d, "opt"))
  expect_true(dir.exists(file.path(d, "opt")))
})

test_that("failed downloads give an informative error and leave nothing", {
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    writeLines("partial", dest)
    stop("cannot open URL")
  })
  cache <- withr::local_tempdir()
  expect_error(
    ts_load_ref(url = "https://example.org/x.csv", cache_dir = cache),
    "Could not download https://example.org/x.csv: cannot open URL"
  )
  expect_length(list.files(cache), 0)
})

test_that("input checks work", {
  expect_error(ts_load_ref(url = "http://example.org/x.csv"), "HTTPS")
  expect_error(ts_load_ref(file = "no/such/file.csv"), "not an existing file")
  expect_error(ts_load_ref(file = 1))
  expect_error(ts_load_ref(file = zip_fixture, refresh = NA))
})

test_that("the result works with ts_resolve_names()", {
  ref <- ts_load_ref(file = zip_fixture)
  res <- ts_resolve_names("Polypodium rotundatum Willd.", ref)
  expect_identical(res$resolved_name, "Polypodium vulgare L.")
})

test_that("zip files are recognized by content, not by name", {
  f <- withr::local_tempfile(fileext = ".dat")
  file.copy(zip_fixture, f)
  expect_true(.ts_is_zip(f))
  expect_identical(nrow(ts_load_ref(file = f)), 3L)
  txt <- withr::local_tempfile(fileext = ".zip")
  writeLines("a,b\n1,2", txt)
  expect_false(.ts_is_zip(txt))
})

test_that("exactly one of source, url or file is needed", {
  expect_error(ts_load_ref(), "exactly one")
  expect_error(ts_load_ref(source = "wfo", file = zip_fixture), "exactly one")
  expect_error(ts_load_ref(source = "nope"), "Unknown `source`")
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

all_sources <- c("wfo", "ppg", "itis", "col", "gbif", "ncbi", "ott", "fb",
  "slb")

taxadb_manifest <- function() {
  providers <- c("itis", "col", "gbif", "ncbi", "ott", "fb", "slb")
  paste(
    c(
      "provider,schema,version,title,rows,upstream_version,license",
      paste0(providers, ',dwc,2026,"', providers, '",10,2026-07-28,CC0 1.0')
    ),
    collapse = "\n"
  )
}

# Pretend answers of the websites that tell us about versions
fake_text <- function(link) {
  if (grepl("zenodo.org/api/records/7460141", link)) {
    zenodo_json(222, "2026-06")
  } else if (grepl("zenodo.org/api/records/222/versions", link)) {
    paste0(
      '{"hits": {"hits": [', zenodo_json(222, "2026-06"), ", ",
      zenodo_json(111, "2025-12"), ", ",
      zenodo_json(110, "2023-06-deprecated"), ", ",
      zenodo_json(100, "1.4.202207"), "]}}"
    )
  } else if (grepl("releases/latest", link, fixed = TRUE)) {
    '{"tag_name": "v2.1.0", "name": "a"}'
  } else if (grepl("api.github.com", link)) {
    paste0(
      '[{"tag_name": "v2.1.0"}, {"tag_name": "v2.0.0"}, ',
      '{"tag_name": "v0.0.0.9009"}]'
    )
  } else if (grepl("list-type=2", link, fixed = TRUE)) {
    paste0(
      "<ListBucketResult><CommonPrefixes><Prefix>taxadb/2026/</Prefix>",
      "</CommonPrefixes><CommonPrefixes><Prefix>taxadb/22.12/</Prefix>",
      "</CommonPrefixes></ListBucketResult>"
    )
  } else if (grepl("taxadb/2026/manifest.csv", link)) {
    taxadb_manifest()
  } else if (grepl("taxadb/22.12/manifest.csv", link)) {
    # an older snapshot, split into many files
    paste0(
      "provider,schema,version,title,rows,parts\n",
      'itis,dwc,22.12,"itis",10,5\n'
    )
  } else {
    stop("unexpected link: ", link)
  }
}

test_that("wfo: latest and specific versions are resolved and recorded", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) {
      if (grepl("versions/latest", link, fixed = TRUE)) {
        zenodo_json(222, "2026-06")
      } else {
        expect_match(link, "records/222/versions")
        paste0('{"hits": {"hits": [', zenodo_json(222, "2026-06"), ", ",
          zenodo_json(111, "2025-12"), "]}}")
      }
    }
  )
  expect_identical(
    .ts_resolve_wfo(NULL)$url,
    "https://zenodo.org/records/222/files/_DwC_backbone_R.zip?download=1"
  )
  expect_identical(.ts_resolve_wfo("2025-12")$version, "2025-12")
  expect_match(.ts_resolve_wfo("2025-12")$url, "records/111/")
  expect_error(.ts_resolve_wfo("1999-01"), "not found")
})

test_that("wfo: tab-separated file with quotes is read and formatted", {
  withr::local_options(taxastand.verbosity = "inform")
  seen <- new.env()
  seen$urls <- character()
  local_mocked_bindings(
    .ts_fetch_text = function(link) zenodo_json(222, "2026-06"),
    .ts_fetch = function(url, dest) {
      seen$urls <- c(seen$urls, url)
      file.copy(test_path("fixtures", "mini-wfo.zip"), dest, overwrite = TRUE)
      invisible(dest)
    }
  )
  cache <- withr::local_tempdir()
  expect_message(ts_load_ref(source = "wfo", cache_dir = cache), "license CC0")
  res <- suppressMessages(ts_load_ref(source = "wfo", cache_dir = cache))
  expect_match(seen$urls, "records/222/")
  # genus and family rows are dropped; the stray quote in the remarks did not
  # stop the reading; wrapping quotes are removed; the author is added
  expect_identical(res$taxonID, c("wfo-3", "wfo-4", "wfo-5", "wfo-6"))
  expect_identical(res$scientificName[1:2], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd."
  ))
  # only the columns that are needed are read
  expect_false("taxonRemarks" %in% colnames(res))
  expect_true("family" %in% colnames(res))
  # unchecked names are kept
  expect_identical(res$taxonomicStatus[4], "unchecked")
  src <- attr(res, "ts_source")
  expect_identical(src$source, "wfo")
  expect_identical(src$version, "2026-06")
  expect_identical(src$license, "CC0 1.0")
  expect_match(src$citation, "zenodo.7460141")

  sub <- ts_load_ref(
    source = "wfo", cache_dir = cache, taxon = "Polypodiaceae"
  )
  expect_identical(sub$taxonID, c("wfo-3", "wfo-4"))
})

test_that("ppg: the latest release is found and the csv is read", {
  seen <- new.env()
  local_mocked_bindings(
    .ts_fetch_text = function(link) {
      '{"url": "x", "tag_name": "v2.1.0", "name": "v"}'
    },
    .ts_fetch = function(url, dest) {
      seen$fetched <- url
      file.copy(test_path("fixtures", "mini-ppg.csv"), dest, overwrite = TRUE)
      invisible(dest)
    }
  )
  res <- ts_load_ref(
    source = "ppg", cache_dir = withr::local_tempdir()
  )
  expect_identical(
    seen$fetched,
    "https://raw.githubusercontent.com/pteridogroup/ppg/v2.1.0/data/ppg.csv"
  )
  expect_identical(res$taxonID, c("wfo-3", "wfo-4", "wfo-5"))
  expect_identical(attr(res, "ts_source")$version, "v2.1.0")
  # a table with only parentNameUsageID is subset by walking down the parents
  sub <- ts_load_ref(
    source = "ppg", version = "v2.0.0", cache_dir = withr::local_tempdir(),
    taxon = "Polypodiaceae"
  )
  expect_identical(sub$taxonID, c("wfo-3", "wfo-4"))
  expect_identical(attr(sub, "ts_source")$version, "v2.0.0")
})

test_that("failure to reach a source gives an informative error", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("cannot open URL", call. = FALSE)
  )
  expect_error(ts_load_ref(source = "ppg"), "cannot open URL")
})

test_that("ts_show_ref_sources() lists the sources", {
  x <- ts_show_ref_sources()
  expect_identical(x$source, all_sources)
  expect_true(all(nzchar(x$license)))
  expect_true(all(nzchar(x$citation)))
})

test_that("versions can be listed (no internet used)", {
  local_mocked_bindings(.ts_fetch_text = fake_text)
  expect_identical(.ts_versions_wfo(), c("2026-06", "2025-12"))
  expect_identical(.ts_versions_ppg(), c("v2.1.0", "v2.0.0", "v0.0.0.9009"))
  # taxadb snapshots split into many files (22.12) are not offered
  expect_identical(.ts_versions_taxadb("itis"), "2026")
  x <- ts_show_ref_sources(versions = TRUE)
  expect_identical(x$source, all_sources)
  expect_identical(x$versions$wfo, c("2026-06", "2025-12"))
  expect_identical(x$versions$ppg, c("v2.1.0", "v2.0.0", "v0.0.0.9009"))
  expect_identical(x$versions$gbif, "2026")
  out <- capture.output(print(x))
  expect_true(any(grepl("^wfo: World Flora Online", out)))
  expect_true(any(grepl("Versions: 2026-06, 2025-12$", out)))
  expect_false("versions" %in% colnames(ts_show_ref_sources()))
  # an unknown version lists the versions that can be used
  expect_error(.ts_resolve_wfo("2020-01"),
    "Versions you can use: 2026-06, 2025-12")
  expect_error(
    .ts_resolve_taxadb("itis", "22.12"), "Versions you can use: 2026"
  )
})

test_that("a source that can't be reached gives NA versions and a message", {
  withr::local_options(taxastand.verbosity = "inform")
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("no internet", call. = FALSE)
  )
  expect_message(ts_show_ref_sources(versions = TRUE), "no internet")
  x <- suppressMessages(ts_show_ref_sources(versions = TRUE))
  expect_true(all(is.na(unlist(x$versions))))
  expect_true(any(grepl("Versions: not available", capture.output(print(x)),
    fixed = TRUE)))
})

test_that("the same failure for several sources is reported once", {
  withr::local_options(taxastand.verbosity = "inform")
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("no internet", call. = FALSE)
  )
  msgs <- testthat::capture_messages(ts_show_ref_sources(versions = TRUE))
  expect_length(msgs, 1)
  expect_match(msgs, "World Flora Online Plant List, .*Integrated Taxonomic")
  expect_match(msgs, "no internet")
})

test_that("a failed download of a named source points to the versions", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) '{"tag_name": "v9.9.9"}',
    .ts_fetch = function(url, dest) stop("HTTP status was '404 Not Found'")
  )
  expect_error(
    ts_load_ref(
      source = "ppg", version = "v9.9.9", cache_dir = withr::local_tempdir()
    ),
    "ts_show_ref_sources\\(versions = TRUE\\)"
  )
})

test_that("long version lists are shortened when printed", {
  x <- ts_show_ref_sources()
  x$versions <- stats::setNames(rep(list("v1"), nrow(x)), x$source)
  x$versions$wfo <- paste0("2020-0", 1:9)
  out <- capture.output(print(x))
  expect_true(any(grepl(
    "Versions: 2020-01, 2020-02, 2020-03, 2020-04, 2020-05, ... (9 in total)",
    out, fixed = TRUE
  )))
  # subsets without all the columns still print
  expect_no_error(capture.output(print(x[, c("source", "versions")])))
})

test_that("tidy output has one row per version", {
  local_mocked_bindings(.ts_fetch_text = fake_text)
  x <- ts_show_ref_sources(versions = TRUE, tidy = TRUE)
  expect_identical(
    x$source, c("wfo", "wfo", "ppg", "ppg", "ppg", all_sources[3:9])
  )
  expect_identical(
    x$version,
    c("2026-06", "2025-12", "v2.1.0", "v2.0.0", "v0.0.0.9009", rep("2026", 7))
  )
  expect_false("versions" %in% colnames(x))
  expect_true(all(c("name", "license", "citation", "url") %in% colnames(x)))
  expect_false(inherits(x, "ts_ref_sources"))
  expect_error(ts_show_ref_sources(tidy = NA))
})

test_that("tidy output keeps a source that could not be reached", {
  withr::local_options(taxastand.verbosity = "inform")
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("no internet", call. = FALSE)
  )
  expect_message(
    ts_show_ref_sources(versions = TRUE, tidy = TRUE), "no internet"
  )
  x <- suppressMessages(ts_show_ref_sources(versions = TRUE, tidy = TRUE))
  expect_identical(x$source, all_sources)
  expect_true(all(is.na(x$version)))
})

test_that("tidy = TRUE does not look up versions unless versions = TRUE", {
  local_mocked_bindings(
    .ts_fetch_text = function(link) stop("should not be called")
  )
  for (x in list(
    ts_show_ref_sources(tidy = TRUE),
    ts_show_ref_sources(versions = FALSE, tidy = TRUE)
  )) {
    expect_identical(x$source, all_sources)
    expect_false(any(c("version", "versions") %in% colnames(x)))
    expect_false(inherits(x, "ts_ref_sources"))
  }
})

test_that("taxadb providers: a Parquet file is downloaded and formatted", {
  withr::local_options(taxastand.verbosity = "inform")
  skip_if_not_installed("nanoparquet")
  seen <- new.env()
  seen$fetched <- character()
  local_mocked_bindings(
    .ts_fetch_text = fake_text,
    .ts_fetch = function(url, dest) {
      seen$fetched <- c(seen$fetched, url)
      file.copy(test_path("fixtures", "mini-taxadb.parquet"), dest,
        overwrite = TRUE)
      invisible(dest)
    }
  )
  cache <- withr::local_tempdir()
  expect_message(
    ts_load_ref(source = "itis", cache_dir = cache), "license CC0"
  )
  res <- suppressMessages(ts_load_ref(source = "itis", cache_dir = cache))
  expect_identical(
    seen$fetched,
      "https://data.source.coop/cboettig/taxadb/2026/dwc_itis_part_0.parquet"
  )
  # the genus is dropped, the author is added, and the synonym with no ID gets
  # one so that it can be resolved
  expect_identical(nrow(res), 4L)
  expect_identical(res$scientificName[1:3], c(
    "Polypodium vulgare L.", "Polypodium rotundatum Willd.",
    "Polypodium vulgare var. alpinum Foo"
  ))
  expect_false(anyNA(res$taxonID))
  expect_false("vernacularName" %in% colnames(res))
  src <- attr(res, "ts_source")
  expect_identical(src$source, "itis")
  expect_identical(src$version, "2026")
  expect_identical(src$upstream_version, "2026-07-28")
  expect_match(src$citation, "10.1111/2041-210X.13440")
  expect_identical(
    ts_resolve_names("Polypodium vulgare var. alpinum Foo", res)$resolved_name,
    "Polypodium vulgare L."
  )
  sub <- ts_load_ref(
    source = "itis", cache_dir = cache, taxon = "Pteridaceae"
  )
  expect_identical(sub$scientificName, "Pteris vittata L.")
})

test_that("a Parquet file can be read from a path", {
  skip_if_not_installed("nanoparquet")
  res <- ts_load_ref(
    file = test_path("fixtures", "mini-taxadb.parquet")
  )
  expect_identical(nrow(res), 4L)
})

test_that("reading Parquet without a reader gives an install hint", {
  local_mocked_bindings(.ts_have_package = function(pkg) FALSE)
  expect_error(
    ts_load_ref(file = test_path("fixtures", "mini-taxadb.parquet")),
    "install.packages\\(\"nanoparquet\"\\)"
  )
})

test_that("duckdb and nanoparquet give the same rows", {
  skip_if_not_installed("nanoparquet")
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  f <- test_path("fixtures", "mini-taxadb.parquet")
  keep <- .ts_sources()$itis$keep
  only <- function(readers) {
    function(pkg) pkg %in% readers
  }
  for (taxon in list("Pteridaceae", c("Pteridaceae", "Polypodiaceae"),
    "Nope")) {
    for (rank in list(NULL, "family")) {
      read_with <- function(readers) {
        local_mocked_bindings(.ts_have_package = only(readers))
        .ts_read_parquet(f, keep, taxon, rank)
      }
      with_np <- read_with("nanoparquet")
      with_duck <- read_with(c("duckdb", "DBI"))
      # duckdb reads fewer rows, but subsetting gives the same result
      expect_lte(nrow(with_duck), nrow(with_np))
      if (!identical(taxon, "Nope")) {
        a <- ts_subset_ref(with_np, taxon, rank = rank)
        b <- ts_subset_ref(with_duck, taxon, rank = rank)
        a <- a[order(a$taxonID), ]
        b <- b[order(b$taxonID), ]
        rownames(a) <- rownames(b) <- NULL
        expect_identical(a, b)
      }
    }
  }
  # the rows read are the ones for the taxon, its synonyms and their accepted
  # names: the synonyms have no family
  local_mocked_bindings(.ts_have_package = only(c("duckdb", "DBI")))
  x <- .ts_read_parquet(f, keep, "Pteridaceae", NULL)
  expect_identical(sort(x$scientificName), c("Pteris", "Pteris vittata"))
  x <- .ts_read_parquet(f, keep, "Polypodiaceae", NULL)
  expect_identical(nrow(x), 3L)
  # all columns of keep that the file has are read, and no others
  expect_false("vernacularName" %in% colnames(x))
  # a rank that is not a column cannot be filtered with SQL: everything is read
  expect_identical(nrow(.ts_read_parquet(f, keep, "Pteris", "genus")), 2L)
  expect_identical(nrow(.ts_read_parquet(f, keep, "Pteris", "species")), 5L)
})

test_that("duckdb alone can read a Parquet file", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  local_mocked_bindings(.ts_have_package = function(pkg) pkg != "nanoparquet")
  f <- test_path("fixtures", "mini-taxadb.parquet")
  expect_identical(nrow(ts_load_ref(file = f)), 4L)
  expect_identical(
    nrow(ts_load_ref(file = f, taxon = "Pteridaceae")), 1L
  )
  # a rank that is not a column: read everything, subset in R
  expect_identical(
    nrow(ts_load_ref(file = f, taxon = "Pteris", rank = "genus")), 1L
  )
})

test_that("duckdb does not create a folder in the home directory", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("DBI")
  skip_if_not("shared_home" %in% names(formals(duckdb::duckdb)))
  home <- withr::local_tempdir()
  withr::local_envvar(HOME = home, USERPROFILE = home)
  local_mocked_bindings(.ts_have_package = function(pkg) pkg != "nanoparquet")
  ts_load_ref(
    file = test_path("fixtures", "mini-taxadb.parquet"), taxon = "Pteridaceae"
  )
  expect_false(dir.exists(file.path(home, ".duckdb")))
})

test_that("unknown taxadb version gives the versions available", {
  local_mocked_bindings(.ts_fetch_text = fake_text)
  expect_error(
    ts_load_ref(source = "gbif", version = "1999"),
    "Versions you can use: 2026"
  )
})

test_that("messages are short, cite once, and can be silenced", {
  withr::local_options(taxastand.verbosity = "inform")
  old <- .ts_state$cited
  withr::defer({
    .ts_state$cited <- old
  })
  .ts_state$cited <- NULL
  local_mocked_bindings(
    .ts_fetch = function(url, dest) {
      file.copy(test_path("fixtures", "mini-ppg.csv"), dest, overwrite = TRUE)
    },
    .ts_resolve_ppg = function(version) {
      list(version = "v1", url = "https://example.org/ppg.csv", extra = list())
    },
    .package = "taxastand"
  )
  cache <- withr::local_tempdir()
  msgs <- testthat::capture_messages(ts_load_ref(source = "ppg",
    cache_dir = cache))
  expect_true(any(grepl("^Loaded .* names from", msgs)))
  expect_true(any(grepl("Please cite", msgs, fixed = TRUE)))
  expect_false(any(grepl("already downloaded", msgs, fixed = TRUE)))
  msgs <- testthat::capture_messages(ts_load_ref(source = "ppg",
    cache_dir = cache))
  expect_false(any(grepl("Please cite", msgs, fixed = TRUE)))
  withr::with_options(
    list(taxastand.verbosity = "quiet"),
    expect_no_message(ts_load_ref(source = "ppg", cache_dir = cache))
  )
})

test_that("the option silences parser warnings in ts_match_names()", {
  withr::local_options(taxastand.verbosity = "inform")
  expect_warning(
    ts_match_names(c("Pteris vittata L.", "!!! ???"), "Pteris vittata L."),
    "could not be parsed"
  )
  withr::with_options(
    list(taxastand.verbosity = "quiet"),
    expect_no_warning(
      ts_match_names(c("Pteris vittata L.", "!!! ???"), "Pteris vittata L.")
    )
  )
})

test_that(".ts_fetch copies a file and turns a failure into an error", {
  src <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", src)
  dest <- withr::local_tempfile()
  link <- paste0("file://", src)
  expect_identical(.ts_fetch(link, dest), dest)
  expect_identical(readLines(dest), "hello")
  expect_error(.ts_fetch(paste0(link, "-missing"), withr::local_tempfile()))
})

test_that("downloads with no file name in the URL, or empty, are handled", {
  cache <- withr::local_tempdir()
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    writeLines("x", dest)
    invisible(dest)
  })
  path <- suppressMessages(.ts_cached_download("#fragment", cache, FALSE))
  expect_match(basename(path), "-download$")
  local_mocked_bindings(.ts_fetch = function(url, dest) {
    file.create(dest)
    invisible(dest)
  })
  expect_error(
    suppressMessages(.ts_cached_download("https://example.org/a.csv", cache, FALSE)),
    "the file is empty"
  )
  expect_length(list.files(cache, pattern = "part$"), 0)
})

test_that(".ts_fetch_text reads a link and gives an informative error", {
  src <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("a", "b"), src)
  expect_identical(.ts_fetch_text(paste0("file://", src)), "a\nb")
  expect_error(
    .ts_fetch_text(paste0("file://", src, "-missing")),
    "Check your internet connection"
  )
})

test_that("a zip file without meta.xml needs exactly one table", {
  dir <- withr::local_tempdir()
  writeLines("a,b", file.path(dir, "one.csv"))
  writeLines("a,b", file.path(dir, "two.csv"))
  expect_error(
    .ts_read_dwca(dir, c("one.csv", "two.csv")),
    "does not hold exactly one table"
  )
  expect_identical(colnames(.ts_read_dwca(dir, "one.csv")), c("a", "b"))
})

test_that("meta.xml without separator or header settings is read", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      '<archive><core rowType="taxon">',
      "<files><location>taxa.csv</location></files>",
      '<id index="0"/>',
      '<field index="1" term="http://rs.tdwg.org/dwc/terms/scientificName"/>',
      "</core></archive>"
    ),
    file.path(dir, "meta.xml")
  )
  writeLines("1,Pteris vittata L.", file.path(dir, "taxa.csv"))
  x <- .ts_read_dwca(dir, c("meta.xml", "taxa.csv"))
  expect_identical(colnames(x), c("taxonID", "scientificName"))
  expect_identical(x$scientificName, "Pteris vittata L.")
})

test_that("columns that are not text are read as text from Parquet", {
  skip_if_not_installed("nanoparquet")
  f <- withr::local_tempfile(fileext = ".parquet")
  nanoparquet::write_parquet(
    data.frame(taxonID = 1:2, scientificName = c("a b", ""), stringsAsFactors = FALSE),
    f
  )
  x <- .ts_read_parquet(f)
  expect_identical(x$taxonID, c("1", "2"))
  expect_identical(x$scientificName, c("a b", NA))
})

test_that("unreadable version information gives informative errors", {
  expect_error(.ts_zenodo_records("{}"), "Could not read the version")
  local_mocked_bindings(.ts_fetch_text = function(link) "{}")
  expect_error(.ts_resolve_ppg(NULL), "Could not find the latest PPG release")
  expect_error(.ts_versions_ppg(), "Could not find any PPG releases")
  .ts_state$taxadb_manifests <- NULL
  expect_error(.ts_taxadb_manifests(refresh = TRUE), "Could not find any taxadb")
})
