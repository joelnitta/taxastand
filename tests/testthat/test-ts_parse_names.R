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
