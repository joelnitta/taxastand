test_that("Input checks work", {
  expect_error(
    ts_match_names(10, "Foogenus"),
    "query must be of class"
  )
  expect_error(
    ts_match_names("Foogenus", 10),
    "reference must be of class"
  )
  expect_error(
    ts_match_names(10, data.frame(genus = "Foogenus")),
    "query must be of class"
  )
  expect_error(
    ts_match_names(data.frame(genus = "Foogenus"), 10),
    "reference must be of class"
  )
})

test_that("Produces expected output", {
  match_res <- ts_match_names(
    "Crepidomanes minutus",
    "Crepidomanes minutum"
  )
  expect_s3_class(match_res, "data.frame")
  expect_snapshot(match_res)
})

test_that("Manually matched names work", {
  match_res <- ts_match_names(
    query = c("Crepidomanes minutus", "Hymeefee erae"),
    reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
    manual_match = data.frame(
      query = "Hymeefee erae",
      match = "Hymenophyllum polyanthos"
    ),
    simple = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Names that can't be parsed don't show up in results", {
  match_res <- ts_match_names(
    query = c(
      "Vanden kalamocarpa x Vanden nipponica x Vanden striata",
      "Crepidomanes minutus"
    ),
    reference = "Crepidomanes minutum",
    simple = TRUE,
    tbl_out = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Manually matched names work with collapsed infrasp names", {
  match_res <- ts_match_names(
    query = c(
      "Crepidomanes minutus",
      "Crepidomanes minutawtaw",
      "Blechnum lunare var. lunare",
      "Blechnum lunare",
      "Bar foo var. foo",
      "Bar foo"
    ),
    reference = c(
      "Crepidomanes minutum",
      "Hymenophyllum polyanthos",
      "Blechnum lunare",
      "Bar foo"
    ),
    manual_match = data.frame(
      query = c("Bar foo var. foo", "Crepidomanes minutawtaw"),
      match = c("Bar foo", "Crepidomanes minutum")
    ),
    max_dist = 10,
    match_no_auth = FALSE,
    match_canon = FALSE,
    collapse_infra = TRUE,
    collapse_infra_exclude = NULL,
    simple = TRUE,
    tbl_out = TRUE
  )
  expect_snapshot(match_res)
})

test_that("strip_qualifiers = TRUE resolves an open-nomenclature qualifier to the right match", {
  match_res <- ts_match_names(
    query = "Tectaria cf. crenata",
    reference = c("Tectaria crenata", "Tectaria subvariolosa"),
    strip_qualifiers = TRUE,
    simple = TRUE
  )
  expect_equal(match_res$reference, "Tectaria crenata")
  expect_equal(match_res$match_type, "exact")
})

test_that("Incorrectly specified manual match fails", {
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = "Hymeefee erae",
        match = "Hymenophyllum poWHAT"
      ),
      simple = TRUE
    ),
    "One or more manually matched reference names not in reference data"
  )
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = c("Crepidomanes minutus", "Crepidomanes minutus"),
        match = c("Hymenophyllum polyanthos", "Crepidomanes minutum")
      ),
      simple = TRUE
    ),
    "All values of manual_match\\$query must be unique"
  )
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        name = c("Hymenophyllum polyantha", "Crepidomanes minutu"),
        match = c("Hymenophyllum polyanthos", "Crepidomanes minutum")
      ),
      simple = TRUE
    ),
    "manual_match must have `query` and `match` columns"
  )
  expect_error(
    ts_match_names(
      query = ts_parse_names("Hymenophyllum polyantha"),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = "Hymenophyllum polyantha",
        match = "Hymenophyllum polyanthos"
      ),
      simple = TRUE
    ),
    "manual_match can only be used if query is a character vector"
  )
})

test_that("ts_match_names accepts pre-parsed query and reference", {
  q <- c("Crepidomanes minutus", "Hymenophyllum polyanthos")
  r <- c("Crepidomanes minutum", "Hymenophyllum polyanthos")
  direct <- ts_match_names(q, r, simple = TRUE)
  parsed <- ts_match_names(
    ts_parse_names(q), ts_parse_names(r), simple = TRUE
  )
  expect_equal(parsed, direct)
  expect_equal(
    ts_match_names(q, ts_parse_names(r), simple = TRUE), direct
  )
})

test_that("ts_match_names validates collapse_infra_exclude", {
  expect_error(
    ts_match_names("Foo bar", "Foo bar", collapse_infra_exclude = 1),
    "character vector"
  )
})

test_that("tbl_out = TRUE returns a tibble", {
  skip_if_not_installed("tibble")
  q <- "Crepidomanes minutus"
  r <- "Crepidomanes minutum"
  expect_s3_class(ts_parse_names(q, tbl_out = TRUE), "tbl_df")
  expect_s3_class(ts_match_names(q, r, tbl_out = TRUE), "tbl_df")
  data(filmy_taxonomy)
  expect_s3_class(
    ts_resolve_names("Gonocormus minutum", filmy_taxonomy, tbl_out = TRUE),
    "tbl_df"
  )
})

test_that("tbl_out = TRUE without tibble installed gives a clear error", {
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_error(
    ts_parse_names("Crepidomanes minutum", tbl_out = TRUE),
    "tibble"
  )
})
