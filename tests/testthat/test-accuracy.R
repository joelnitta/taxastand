# Hand-labelled accuracy fixture.
#
# Expected values here are what a taxonomist would say is CORRECT, not a
# snapshot of current output. They are the ground truth for "accurate" and the
# regression guard for performance work. Cases the package cannot yet handle
# are marked with skip() and the issue that will fix them.

ref <- data.frame(
  taxonID = as.character(1:8),
  scientificName = c(
    "Pteridium aquilinum (L.) Kuhn", # accepted
    "Pteris aquilina L.", # synonym of 1
    "Asplenium nidus L.", # accepted
    "Hymenophyllum taiwanense (Tagawa) C.V.Morton", # accepted
    "Hymenophyllum taiwanense Tagawa", # same epithet, different author
    "Cyathea medullaris (G.Forst.) Sw.", # accepted
    "Polypodium vulgare L.", # accepted
    "Polypodium vulgare subsp. prionodes (Asch.) Rothm." # accepted
  ),
  taxonomicStatus = c(
    "accepted", "synonym", "accepted", "accepted", "synonym",
    "accepted", "accepted", "accepted"
  ),
  acceptedNameUsageID = c(NA, "1", NA, NA, "4", NA, NA, NA),
  stringsAsFactors = FALSE
)

# Resolve one query; return the resolved name (NA if unresolved)
resolve_one <- function(query, ...) {
  res <- suppressWarnings(
    ts_resolve_names(query, ref, max_dist = 3, ...)
  )
  as.character(res$resolved_name[res$query == query])
}

test_that("exact matches resolve to themselves", {
  expect_equal(resolve_one("Asplenium nidus L."), "Asplenium nidus L.")
  expect_equal(
    resolve_one("Cyathea medullaris (G.Forst.) Sw."),
    "Cyathea medullaris (G.Forst.) Sw."
  )
})

test_that("synonyms resolve to the accepted name", {
  expect_equal(
    resolve_one("Pteris aquilina L."),
    "Pteridium aquilinum (L.) Kuhn"
  )
  expect_equal(
    resolve_one("Hymenophyllum taiwanense Tagawa"),
    "Hymenophyllum taiwanense (Tagawa) C.V.Morton"
  )
})

test_that("author differences (punctuation, spacing) still match", {
  expect_equal(
    resolve_one("Asplenium nidus L"),
    "Asplenium nidus L."
  )
  expect_equal(
    resolve_one("Cyathea medullaris (G. Forst.) Sw."),
    "Cyathea medullaris (G.Forst.) Sw."
  )
})

test_that("a missing basionym author is tolerated", {
  expect_equal(
    resolve_one("Cyathea medullaris Sw."),
    "Cyathea medullaris (G.Forst.) Sw."
  )
})

test_that("misspelled epithets with the right author match", {
  expect_equal(
    resolve_one("Pteridium aquilinim (L.) Kuhn"),
    "Pteridium aquilinum (L.) Kuhn"
  )
  expect_equal(
    resolve_one("Asplenium nidis L."),
    "Asplenium nidus L."
  )
})

test_that("the author is used to tell apart otherwise identical names", {
  # Same genus + epithet, different authors: must not collapse to wrong name
  expect_equal(
    resolve_one("Hymenophyllum taiwanense (Tagawa) C.V.Morton"),
    "Hymenophyllum taiwanense (Tagawa) C.V.Morton"
  )
})

test_that("infraspecific names are not confused with their species", {
  expect_equal(
    resolve_one("Polypodium vulgare subsp. prionodes (Asch.) Rothm."),
    "Polypodium vulgare subsp. prionodes (Asch.) Rothm."
  )
  expect_equal(resolve_one("Polypodium vulgare L."), "Polypodium vulgare L.")
})

test_that("names absent from the reference are not forced to a match", {
  expect_true(is.na(resolve_one("Zzyzx completely-different Foo")))
  # Different author and distant spelling must not match
  expect_true(is.na(resolve_one("Cyathea medullaris Hook.")))
})

test_that("author-less queries are fuzzy-matched on the canonical name", {
  # The author string must not count towards the distance
  expect_equal(
    resolve_one("Pteridium aquilinim"),
    "Pteridium aquilinum (L.) Kuhn"
  )
  # ... and an author-less canonical match works too
  expect_equal(
    resolve_one("Pteridium aquilinum"),
    "Pteridium aquilinum (L.) Kuhn"
  )
  expect_equal(resolve_one("Asplenium nidus"), "Asplenium nidus L.")
})

test_that("author-less canonical fuzzy matching respects max_dist", {
  res <- suppressWarnings(
    ts_match_names("Pteridium aquilinim", ref$scientificName, max_dist = 0)
  )
  expect_equal(res$match_type, "no_match")
})

test_that("misspelled genus matches", {
  expect_equal(resolve_one("Aspelnium nidus L."), "Asplenium nidus L.")
  # author-less
  expect_equal(resolve_one("Aspelnium nidus"), "Asplenium nidus L.")
  res <- suppressWarnings(
    ts_match_names("Aspelnium nidus L.", ref$scientificName, simple = TRUE)
  )
  expect_equal(res$match_type, "auto_fuzzy_genus")
  # can be switched off
  res0 <- suppressWarnings(
    ts_match_names(
      "Aspelnium nidus L.", ref$scientificName,
      max_dist_genus = 0, simple = TRUE
    )
  )
  expect_equal(res0$match_type, "no_match")
})

test_that("a swap of adjacent letters is one edit", {
  # "Asplenium ndius" needs 2 plain edits but is one swap away
  res <- suppressWarnings(
    ts_match_names("Asplenium ndius L.", ref$scientificName, max_dist = 1)
  )
  expect_equal(res$reference, "Asplenium nidus L.")
  expect_equal(res$dist, 1L)
  res0 <- suppressWarnings(
    ts_match_names(
      "Asplenium ndius L.", ref$scientificName,
      max_dist = 1, transpositions = FALSE
    )
  )
  expect_equal(res0$match_type, "no_match")
})

test_that("names in other genera in the reference do not change results", {
  # Reference rows are pre-filtered to the query genera for speed; results
  # must be the same as when the reference contains only that genus.
  only_genus <- ref[grepl("^Asplenium", ref$scientificName), ]
  a <- suppressWarnings(
    ts_resolve_names("Asplenium nidis L.", ref, max_dist = 3)
  )
  b <- suppressWarnings(
    ts_resolve_names("Asplenium nidis L.", only_genus, max_dist = 3)
  )
  expect_equal(a, b)
})

test_that("a query list identical to the reference still resolves", {
  # (IDs are derived from a hash of the input; they must not collide)
  res <- suppressWarnings(
    ts_resolve_names("Asplenium nidus L.", ref[ref$scientificName == "Asplenium nidus L.", ])
  )
  expect_equal(res$resolved_name, "Asplenium nidus L.")
  expect_equal(res$match_type, "exact")
})

test_that("fuzzy_canon_hits = 'closest' resolves ambiguity from distant hits", {
  extra <- data.frame(
    taxonID = c("101", "102"),
    scientificName = c("Pteridium aquilxnxm Foo", "Pteridium aquilinaxx Bar"), # distance 2, 3
    taxonomicStatus = "accepted",
    acceptedNameUsageID = NA,
    stringsAsFactors = FALSE
  )
  ref2 <- rbind(ref, extra)
  q <- "Pteridium aquilinim"
  all_hits <- suppressWarnings(
    ts_match_names(q, ref2$scientificName, max_dist = 3)
  )
  closest <- suppressWarnings(
    ts_match_names(q, ref2$scientificName, max_dist = 3, fuzzy_canon_hits = "closest")
  )
  expect_equal(nrow(all_hits), 3)
  expect_equal(closest$reference, "Pteridium aquilinum (L.) Kuhn")
  expect_error(
    ts_match_names(q, ref2$scientificName, fuzzy_canon_hits = "none"),
    "should be one of"
  )
})
