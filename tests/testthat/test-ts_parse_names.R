test_that("Input checks work", {
  expect_error(
    ts_parse_names(c("Foogenus","Foogenus")),
    "Input taxa must be unique"
  )
  expect_error(
    ts_parse_names(c("Foogenus", NA)),
    "Input taxa may not contain NAs"
  )
})

test_that("Parsing works with docker", {
  skip_if_no_docker()
  expect_snapshot(
    ts_parse_names(
      "Foogenus x barspecies var. foosubsp (L.) F. Bar",
      docker = TRUE)
  )
})

test_that("Parsing works with local taxon-tools", {
  skip_if_no_tt()
  expect_snapshot(
    ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
  )
})
