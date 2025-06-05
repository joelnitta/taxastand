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

test_that("Produces expected output in docker", {
  skip_if_no_docker()
  match_res <- ts_match_names(
    "Crepidomanes minutus",
    "Crepidomanes minutum",
    docker = TRUE
  )
  expect_s3_class(match_res, "data.frame")
  expect_snapshot(match_res)
})

test_that("Produces expected output without docker", {
  skip_if_no_tt()
  match_res <- ts_match_names(
    "Crepidomanes minutus",
    "Crepidomanes minutum"
  )
  expect_s3_class(match_res, "data.frame")
  expect_snapshot(match_res)
})
