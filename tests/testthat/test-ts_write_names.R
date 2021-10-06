test_that("Input checks work", {
  expect_error(
    ts_write_names("Foogenus", tempfile()),
    "df must be of class 'data\\.frame'"
  )
  partial_names_df <- data.frame(
    id = "1",
    genus_hybrid_sign = "x"
  )
  expect_error(
    ts_write_names(partial_names_df, tempfile()),
    "df must include the following columns"
  )
})

test_that("Produces expected output file", {
  parsed_names <- ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
  expect_snapshot_file(ts_write_names(parsed_names, "parsed_name.txt"), "parsed_name.txt")
  file.remove("parsed_name.txt")
})
