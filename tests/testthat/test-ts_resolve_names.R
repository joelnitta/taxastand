data(filmy_taxonomy)

test_that("Input checks work", {
  expect_error(
    ts_resolve_names(10, data.frame(genus = "Foogenus",
      stringsAsFactors = FALSE)),
    "query must be of class"
  )
  expect_error(
    ts_resolve_names(data.frame(genus = "Foogenus", stringsAsFactors = FALSE),
      10),
    "ref must be of class"
  )
})

test_that("Produces expected output", {
  # Query a misspelled name
  match_results <- ts_match_names(
    query = "Gonocormus minutum",
    ref = unique(filmy_taxonomy$scientificName),
    simple = TRUE
  )
  expect_s3_class(
    ts_resolve_names(match_results, filmy_taxonomy),
    "data.frame"
  )
  expect_s3_class(
    ts_resolve_names("Gonocormus minutum", filmy_taxonomy),
    "data.frame"
  )
  expect_snapshot(match_results)
})

test_that("strip_qualifiers = TRUE resolves a name with an open qualifier", {
  resolved <- ts_resolve_names(
    "Gonocormus cf. minutum",
    filmy_taxonomy,
    strip_qualifiers = TRUE
  )
  expect_identical(resolved$resolved_name,
    "Crepidomanes minutum (Bl.) K. Iwats.")
})

test_that("ts_resolve_names validates match_autonyms_exclude", {
  data(filmy_taxonomy)
  expect_error(
    ts_resolve_names(
      "Gonocormus minutum", filmy_taxonomy, match_autonyms_exclude = 1
    ),
    "character vector"
  )
})

test_that("ts_resolve_names accepts collapse_infra as an alias", {
  data(filmy_taxonomy)
  q <- c("Hymenophyllum polyanthos var. polyanthos", "Gonocormus minutum")
  expect_identical(
    suppressWarnings(ts_resolve_names(q, filmy_taxonomy,
      collapse_infra = TRUE)),
    suppressWarnings(ts_resolve_names(q, filmy_taxonomy, match_autonyms = TRUE))
  )
  expect_error(
    ts_resolve_names(
      q, filmy_taxonomy, match_autonyms = TRUE, collapse_infra = FALSE
    ),
    "alias of `match_autonyms`"
  )
})

test_that("`ref_taxonomy` still works as an alias of `ref`", {
  q <- "Gonocormus minutum"
  expect_identical(
    ts_resolve_names(q, ref_taxonomy = filmy_taxonomy),
    ts_resolve_names(q, ref = filmy_taxonomy)
  )
  expect_error(
    ts_resolve_names(q, ref = filmy_taxonomy,
      ref_taxonomy = filmy_taxonomy[1:3, ]),
    "alias of `ref`"
  )
})
