test_that("Input checks work", {
  expect_error(
    ts_parse_names(c("Foogenus", "Foogenus")),
    "Input taxa must be unique"
  )
  expect_error(
    ts_parse_names(c("Foogenus", NA)),
    "Input taxa may not contain NAs"
  )
})

test_that("Parsing works", {
  expect_snapshot(
    ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
  )
})

test_that("strip_qualifiers = FALSE (default) reproduces mis-parse of open-nomenclature qualifiers", {
  cf_parsed <- ts_parse_names("Tectaria cf. crenata", quiet = TRUE)
  expect_equal(cf_parsed$specific_epithet, "cf")
  expect_equal(cf_parsed$author, ". crenata")

  aff_parsed <- ts_parse_names("Huperzia aff. liangshanica", quiet = TRUE)
  expect_equal(aff_parsed$specific_epithet, "aff")
  expect_equal(aff_parsed$author, ". liangshanica")
})

test_that("strip_qualifiers = TRUE strips cf. and aff. before parsing", {
  cf_parsed <- ts_parse_names(
    "Tectaria cf. crenata",
    strip_qualifiers = TRUE
  )
  expect_equal(cf_parsed$specific_epithet, "crenata")
  expect_true(is.na(cf_parsed$author))

  aff_parsed <- ts_parse_names(
    "Huperzia aff. liangshanica",
    strip_qualifiers = TRUE
  )
  expect_equal(aff_parsed$specific_epithet, "liangshanica")
  expect_true(is.na(aff_parsed$author))
})

test_that("strip_qualifiers = TRUE strips a qualifier before an infraspecific epithet", {
  parsed <- ts_parse_names(
    "Foogenus barspecies aff. var. foosubsp",
    strip_qualifiers = TRUE
  )
  expect_equal(parsed$specific_epithet, "barspecies")
  expect_equal(parsed$infraspecific_rank, "var.")
  expect_equal(parsed$infraspecific_epithet, "foosubsp")
})

test_that("strip_qualifiers = TRUE strips trailing sensu lato/stricto qualifiers", {
  sl_parsed <- ts_parse_names(
    "Foogenus barspecies s.l.",
    strip_qualifiers = TRUE
  )
  expect_equal(sl_parsed$specific_epithet, "barspecies")
  expect_true(is.na(sl_parsed$author))

  sstr_parsed <- ts_parse_names(
    "Foogenus barspecies sensu stricto",
    strip_qualifiers = TRUE
  )
  expect_equal(sstr_parsed$specific_epithet, "barspecies")
  expect_true(is.na(sstr_parsed$author))
})
