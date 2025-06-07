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

test_that("Manually matched names work", {
  skip_if_no_docker()
  match_res <- ts_match_names(
    query = c("Crepidomanes minutus", "Hymeefee erae"),
    reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
    manual_match = data.frame(
      query = "Hymeefee erae",
      match = "Hymenophyllum polyanthos"
    ),
    simple = TRUE,
    docker = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Manually matched names work with collapsed infrasp names", {
  skip_if_no_docker()
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
    docker = TRUE,
    tbl_out = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Incorrectly specified manual match fails", {
  skip_if_no_docker()
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = "Hymeefee erae",
        match = "Hymenophyllum poWHAT"
      ),
      simple = TRUE,
      docker = TRUE
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
      simple = TRUE,
      docker = TRUE
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
      simple = TRUE,
      docker = TRUE
    ),
    "manual_match must have `query` and `match` columns"
  )
  expect_error(
    ts_match_names(
      query = ts_parse_names("Hymenophyllum polyantha", docker = TRUE),
      reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = c("Hymenophyllum polyantha"),
        match = c("Hymenophyllum polyanthos")
      ),
      simple = TRUE,
      docker = TRUE
    ),
    "manual_match can only be used if query is a character vector"
  )
})
