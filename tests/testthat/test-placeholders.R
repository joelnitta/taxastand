test_that("placeholder names are detected", {
  x <- c(
    "Pteris sp.", "Pteris spp.", "Pteris sp", "Pteris indet.", "Pteris stet.",
    "Pteris sp. inc.", "Pteris sp. indet.", "Pteris vittata L.",
    "Pteris spinosa L.", "Pteris incana Sw.", "Pteris vittata subsp. spicata"
  )
  expect_equal(
    taxastand:::.ts_is_placeholder(x),
    c(rep(TRUE, 7), rep(FALSE, 4))
  )
})

test_that("ts_parse_names excludes placeholders with a warning", {
  withr::local_options(taxastand.verbosity = "inform")
  expect_warning(
    res <- ts_parse_names(c("Pteris sp.", "Pteris vittata L.")),
    "placeholders"
  )
  expect_equal(res$name, "Pteris vittata L.")
  res2 <- ts_parse_names(
    c("Pteris sp.", "Pteris vittata L."),
    exclude_placeholders = FALSE
  )
  expect_equal(nrow(res2), 2)
})

test_that("placeholder queries are not fuzzy-matched", {
  withr::local_options(taxastand.verbosity = "inform")
  ref <- c("Pteris vittata L.", "Pteris sp. L.")
  expect_warning(
    res <- ts_match_names(
      c("Pteris sp.", "Pteris vittata L."),
      ref,
      simple = TRUE
    ),
    "placeholders"
  )
  expect_equal(res$query, "Pteris vittata L.")
})
