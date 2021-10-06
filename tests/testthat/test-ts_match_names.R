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
  expect_s3_class(
    ts_match_names("Crepidomanes minutus", "Crepidomanes minutum"),
    "data.frame")
})
