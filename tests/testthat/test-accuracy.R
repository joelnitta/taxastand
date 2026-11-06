# Hand-labelled accuracy fixture.
#
# Expected values here are what a taxonomist would say is CORRECT, not a
# snapshot of current output. They are the ground truth for "accurate" and the
# regression guard for performance work. Cases the package cannot yet handle
# are marked with skip() and the issue that will fix them.

ref <- data.frame(
  taxonID = as.character(1:8),
  scientificName = c(
    "Crepidomanes minutum (Blume) K.Iwats.", # accepted
    "Trichomanes minutum Blume", # synonym of 1
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
    resolve_one("Trichomanes minutum Blume"),
    "Crepidomanes minutum (Blume) K.Iwats."
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
    resolve_one("Crepidomanes minutom (Blume) K.Iwats."),
    "Crepidomanes minutum (Blume) K.Iwats."
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

test_that("author-less query with a misspelled epithet matches (known gap)", {
  skip("Known gap: author-less fuzzy matching, see #49")
  expect_equal(
    resolve_one("Crepidomanes minutom"),
    "Crepidomanes minutum (Blume) K.Iwats."
  )
})

test_that("misspelled genus matches (known gap)", {
  skip("Known gap: genus-level fuzzy matching, see #50")
  expect_equal(resolve_one("Aspelnium nidus L."), "Asplenium nidus L.")
})
