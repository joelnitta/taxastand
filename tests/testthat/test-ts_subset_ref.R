x_ranks <- data.frame(
  taxonID = c("1", "2", "3", "4", "5", "6"),
  acceptedNameUsageID = c("1", "2", "3", "1", "5", "2"),
  taxonomicStatus = c(
    "accepted", "accepted", "accepted", "synonym", "accepted", "synonym"
  ),
  scientificName = c(
    "Polypodium vulgare L.", "Pteris vittata L.", "Pteris cretica L.",
    "Polypodium rotundatum Willd.", "Equisetum arvense L.",
    "Ceratopteris vittata (L.) Foo"
  ),
  family = c("Polypodiaceae", "Pteridaceae", "Pteridaceae", NA, "Equisetaceae",
    NA),
  stringsAsFactors = FALSE
)

test_that("subsetting by a higher-rank column keeps members and synonyms", {
  res <- ts_subset_ref(x_ranks, "Pteridaceae")
  expect_identical(res$taxonID, c("2", "3", "6"))
  expect_identical(colnames(res), colnames(x_ranks))
  # synonym of a Polypodiaceae name is found through acceptedNameUsageID
  res2 <- ts_subset_ref(x_ranks, "Polypodiaceae")
  expect_identical(res2$taxonID, c("1", "4"))
})

test_that("several taxa can be kept at once", {
  res <- ts_subset_ref(x_ranks, c("Polypodiaceae", "Equisetaceae"))
  expect_identical(res$taxonID, c("1", "4", "5"))
})

test_that("accepted names of kept synonyms are added", {
  x <- x_ranks
  x$family[6] <- "Pteridaceae"
  x$family[2] <- "Other"
  res <- ts_subset_ref(x, "Pteridaceae")
  expect_true("2" %in% res$taxonID)
})

test_that("rank restricts which column matches", {
  x <- x_ranks
  x$genus <- c("Polypodium", "Pteris", "Pteris", NA, "Equisetum", NA)
  expect_identical(ts_subset_ref(x, "Pteris", rank = "genus")$taxonID, c("2",
    "3", "6"))
  expect_error(ts_subset_ref(x, "Pteris", rank = "family"), "No|None")
})

test_that("parentNameUsageID is used when there are no rank columns", {
  x <- data.frame(
    taxonID = c("1", "2", "3", "4", "5"),
    acceptedNameUsageID = c("1", "2", "3", "4", "2"),
    taxonomicStatus = c("accepted", "accepted", "accepted", "accepted",
      "synonym"),
    scientificName = c(
      "Pteridaceae E.D.M.Kirchn.", "Pteris L.", "Pteris vittata L.",
      "Equisetum L.", "Ceratopteris vittata Foo"
    ),
    taxonRank = c("family", "genus", "species", "genus", "species"),
    parentNameUsageID = c(NA, "1", "2", NA, NA),
    stringsAsFactors = FALSE
  )
  res <- ts_subset_ref(x, "Pteridaceae")
  expect_identical(res$taxonID, c("1", "2", "3", "5"))
  expect_identical(ts_subset_ref(x, "Pteris", rank = "genus")$taxonID, c("2",
    "3", "5"))
  expect_error(ts_subset_ref(x, "Pteridaceae", rank = "genus"), "None")
})

test_that("errors and warnings", {
  expect_error(ts_subset_ref(x_ranks, "Nope"), "None of")
  expect_warning(
    ts_subset_ref(x_ranks, c("Pteridaceae", "Nope")), "Not found in `ref`: Nope"
  )
  expect_error(ts_subset_ref(x_ranks[, 1:4], "Pteridaceae"),
    "no columns for higher ranks")
  expect_error(ts_subset_ref(x_ranks[, -1], "Pteridaceae"), "missing required")
  expect_error(ts_subset_ref(x_ranks, character(0)))
  expect_error(ts_subset_ref(1, "a"))
})

test_that("result works with ts_format_ref()", {
  ref <- ts_format_ref(ts_subset_ref(x_ranks, "Pteridaceae"))
  expect_identical(nrow(ref), 3L)
  expect_true(all(c("taxonID", "scientificName") %in% colnames(ref)))
})

test_that("col_map maps columns and the original columns are returned", {
  x <- x_ranks
  colnames(x) <- c("id", "acc", "status", "name", "fam")
  map <- c(taxonID = "id", acceptedNameUsageID = "acc", family = "fam")
  res <- ts_subset_ref(x, "Pteridaceae", col_map = map)
  expect_identical(res$id, c("2", "3", "6"))
  expect_identical(colnames(res), colnames(x))
  expect_error(ts_subset_ref(x, "Pteridaceae", col_map = c(family = "zzz")),
    "not in `ref`")
})
