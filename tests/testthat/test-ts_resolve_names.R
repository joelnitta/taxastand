test_that("Input checks work", {
  expect_error(
    ts_resolve_names(10, data.frame(genus = "Foogenus")),
    "query must be of class"
  )
  expect_error(
    ts_resolve_names(data.frame(genus = "Foogenus"), 10),
    "ref_taxonomy must be of class"
  )
})

test_that("Produces expected output", {
  data(filmy_taxonomy)
  # Query a misspelled name
  match_results <- ts_match_names(
    query = "Gonocormus minutum",
    reference = unique(filmy_taxonomy$scientificName),
    simple = TRUE)
  expect_s3_class(
    ts_resolve_names(match_results, filmy_taxonomy),
    "data.frame")
  expect_s3_class(
    ts_resolve_names("Gonocormus minutum", filmy_taxonomy),
    "data.frame")
})

