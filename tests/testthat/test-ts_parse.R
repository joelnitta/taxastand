test_that("Input checks work", {
  expect_error(
    ts_parse(c("Foogenus","Foogenus")),
    "Input taxa must be unique"
  )
  expect_error(
    ts_parse(c("Foogenus", NA)),
    "Input taxa may not contain NAs"
  )
})
