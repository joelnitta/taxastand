test_that("Making a dataframe with taxonomic names works", {
  expect_s3_class(
    ts_make_name_df("Foogenus x barspecies var. foosubsp (L.) F. Bar"),
    "data.frame")
  expect_error(
    ts_make_name_df(c("Foogenus","Foogenus")),
    "Input taxa must be unique"
  )
  expect_error(
    ts_make_name_df(c("Foogenus", NA)),
    "Input taxa may not contain NAs"
  )
})
