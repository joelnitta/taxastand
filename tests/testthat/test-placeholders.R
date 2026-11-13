test_that("placeholder names are detected", {
  x <- c(
    "Pteris sp.", "Pteris spp.", "Pteris sp", "Pteris indet.", "Pteris stet.",
    "Pteris sp. inc.", "Pteris sp. indet.", "Pteris vittata L.",
    "Pteris spinosa L.", "Pteris incana Sw.", "Pteris vittata subsp. spicata"
  )
  expect_identical(
    .ts_is_placeholder(x),
    c(rep(TRUE, 7), rep(FALSE, 4))
  )
})

test_that("ts_parse_names excludes placeholders with a warning", {
  withr::local_options(taxastand.verbosity = "inform")
  expect_warning(
    ts_parse_names(c("Pteris sp.", "Pteris vittata L.")),
    "placeholders"
  )
  res <- suppressWarnings(ts_parse_names(c("Pteris sp.", "Pteris vittata L.")))
  expect_identical(res$name, "Pteris vittata L.")
  res2 <- ts_parse_names(
    c("Pteris sp.", "Pteris vittata L."),
    exclude_placeholders = FALSE
  )
  expect_identical(nrow(res2), 2L)
})

test_that("placeholder queries are not fuzzy-matched", {
  withr::local_options(taxastand.verbosity = "inform")
  ref <- c("Pteris vittata L.", "Pteris sp. L.")
  match_placeholders <- function() {
    ts_match_names(c("Pteris sp.", "Pteris vittata L."), ref, simple = TRUE)
  }
  expect_warning(match_placeholders(), "placeholders")
  res <- suppressWarnings(match_placeholders())
  expect_identical(res$query, "Pteris vittata L.")
})
