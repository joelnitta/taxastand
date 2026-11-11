raw <- data.frame(
  id = c("1", "2", "3", "4", "5", "1"),
  acc = c("1", "1", "3", "", "5", "1"),
  status = c(" Accepted", "SYNONYM", "accepted", "accepted", "accepted", "accepted"),
  name = c(
    "Polypodium vulgare", "Polypodium rotundatum", "Polypodium",
    "Polypodiaceae", "Asplenium nidus L.", "Polypodium vulgare"
  ),
  author = c("L.", "Willd.", "L.", "", "L.", "L."),
  rank = c("Species", "species", "GENUS", "family", "species", "SPECIES"),
  stringsAsFactors = FALSE
)
map <- c(
  taxonID = "id", acceptedNameUsageID = "acc", taxonomicStatus = "status",
  scientificName = "name", scientificNameAuthorship = "author",
  taxonRank = "rank"
)

test_that("ts_format_ref() maps columns, adds authors, drops higher ranks", {
  out <- ts_format_ref(raw, col_map = map)
  expect_s3_class(out, "data.frame")
  expect_equal(
    colnames(out),
    c("taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName",
      "taxonRank")
  )
  # genus and family dropped; row 6 duplicates row 1 once tidied
  expect_equal(out$taxonID, c("1", "2", "5"))
  expect_equal(
    out$scientificName,
    c("Polypodium vulgare L.", "Polypodium rotundatum Willd.",
      "Asplenium nidus L.")
  )
  expect_equal(out$taxonomicStatus, c("accepted", "synonym", "accepted"))
  expect_equal(out$taxonRank, rep("species", 3))
})

test_that("an author already in the name is not added twice", {
  out <- ts_format_ref(raw, col_map = map)
  expect_equal(out$scientificName[3], "Asplenium nidus L.")
})

test_that("empty strings become NA", {
  raw2 <- raw
  raw2$rank <- NULL
  out <- ts_format_ref(raw2, col_map = map[names(map) != "taxonRank"])
  expect_true(is.na(out$acceptedNameUsageID[out$taxonID == "4"]))
})

test_that("only ranks above species are dropped, other ranks are kept", {
  x <- data.frame(
    taxonID = as.character(1:4), acceptedNameUsageID = NA_character_,
    taxonomicStatus = "accepted",
    scientificName = c("A b", "A b subsp. c", "A b nothovar. d", "A"),
    taxonRank = c("species", "nothosubspecies", NA, "subgenus")
  )
  out <- ts_format_ref(x)
  expect_equal(out$taxonID, c("1", "2", "3"))
  expect_equal(nrow(ts_format_ref(x, drop_ranks = NULL)), 4)
  expect_equal(
    ts_format_ref(x, drop_ranks = "nothosubspecies")$taxonID,
    c("1", "3", "4")
  )
})

test_that("no taxonRank column means no rank filtering", {
  x <- data.frame(
    taxonID = c("1", "2"), acceptedNameUsageID = NA_character_,
    taxonomicStatus = "accepted", scientificName = c("A b", "A")
  )
  expect_equal(nrow(ts_format_ref(x)), 2)
})

test_that("source is kept as an attribute", {
  src <- list(provider = "Example", version = "1")
  expect_equal(attr(ts_format_ref(raw, map, source = src), "ts_source"), src)
  expect_null(attr(ts_format_ref(raw, map), "ts_source"))
})

test_that("a mapped column replaces a column with the same Darwin Core name", {
  x <- cbind(raw, taxonID = c("a", "b", "c", "d", "e", "f"))
  expect_equal(ts_format_ref(x, map)$taxonID, c("1", "2", "5"))
})

test_that("the result works as a reference", {
  out <- ts_format_ref(raw, map)
  res <- ts_resolve_names("Polypodium rotundatum Willd.", out)
  expect_equal(res$resolved_name, "Polypodium vulgare L.")
  expect_s3_class(ts_optimize_ref(out), "ts_reference")
})

test_that("ts_format_ref() checks its input", {
  expect_error(ts_format_ref(1:3), "data.frame")
  expect_error(ts_format_ref(raw), "missing required columns")
  expect_error(
    ts_format_ref(raw, c(taxonID = "nope")), "not in `ref`"
  )
  expect_error(ts_format_ref(raw, c("id", "acc")), "named character vector")
  expect_error(ts_format_ref(raw, map, source = "x"))
})

test_that("hierarchy columns are kept, other columns are dropped", {
  x <- data.frame(
    taxonID = c("1", "2"), acceptedNameUsageID = c("1", "1"),
    taxonomicStatus = c("accepted", "synonym"),
    scientificName = c("Pteris vittata L.", "Ceratopteris vittata Foo"),
    family = c("Pteridaceae", NA), parentNameUsageID = c("9", NA),
    extra = c("a", "b")
  )
  res <- ts_format_ref(x)
  expect_equal(
    colnames(res),
    c(
      "taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName",
      "family", "parentNameUsageID"
    )
  )
})

test_that("ts_subset_ref() works after ts_format_ref() for rank columns", {
  x <- data.frame(
    taxonID = c("1", "2", "3"), acceptedNameUsageID = c("1", "2", "1"),
    taxonomicStatus = c("accepted", "accepted", "synonym"),
    scientificName = c("Pteris vittata L.", "Equisetum arvense L.", "Foo bar"),
    family = c("Pteridaceae", "Equisetaceae", NA)
  )
  res <- ts_subset_ref(ts_format_ref(x), "Pteridaceae")
  expect_equal(res$taxonID, c("1", "3"))
})
