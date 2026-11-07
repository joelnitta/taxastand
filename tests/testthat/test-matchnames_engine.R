test_that("tt_depunct handles mixed ASCII and non-ASCII input", {
  x <- c("Plain (L.) Name", "Café Müll.", "A×b", "", "plain")
  expect_equal(
    tt_depunct(x),
    c("plainlname", "cafemull", "a×b", "", "plain")
  )
  # Result must not depend on what else is in the vector
  expect_equal(tt_depunct(x[2]), tt_depunct(x)[2])
})

test_that("tt_matchnames returns the documented columns", {
  ref <- c("r1||Foo||bar|||L.", "r2||Foo||baz|||Sm.")
  res <- tt_matchnames("q1||Foo||bar|||L.", ref)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1)
  expect_equal(res$match_type, "exact")
  expect_equal(res$id_ref, "r1")
  expect_equal(res$author_ref, "L.")
  expect_true(is.na(res$infraspecific_rank_ref))
  expect_equal(ncol(res), 18)
  expect_true(is.na(res$dist))
})

test_that("tt_matchnames: last reference record wins for duplicate keys", {
  ref <- c("r1||Foo||bar|||L.", "r2||Foo||bar|||L.")
  res <- tt_matchnames("q1||Foo||bar|||L.", ref)
  expect_equal(res$id_ref, "r2")
})

test_that("tt_matchnames: unmatched query gives one no_match row", {
  res <- tt_matchnames(
    c("q1||Zzz||yyy|||X.", "q2||Foo||bar|||L."),
    "r1||Foo||bar|||L."
  )
  expect_equal(res$match_type, c("no_match", "exact"))
  expect_true(is.na(res$id_ref[1]))
  expect_true(is.na(res$genus_name_ref[1]))
})

test_that("tt_matchnames: fuzzy matches can return several rows per query", {
  ref <- c("r1||Foo||bart|||L.", "r2||Foo||barn|||L.", "r3||Foo||zzzzzz|||L.")
  res <- tt_matchnames("q1||Foo||bar|||L.", ref, max_dist = 3)
  expect_equal(res$match_type, c("auto_fuzzy", "auto_fuzzy"))
  expect_setequal(res$id_ref, c("r1", "r2"))
})

test_that("tt_matchnames: fuzzy hits are returned in reference order", {
  ref <- c("r9||Foo||barn|||L.", "r10||Foo||bart|||L.", "r2||Foo||barx|||L.")
  res <- tt_matchnames("q1||Foo||bar|||L.", ref, max_dist = 3)
  expect_equal(res$id_ref, c("r9", "r10", "r2"))
})

test_that(".tt_split_field splits parser output, incl. short and empty fields", {
  x <- c("id1|×|Foo|||bar||L.", "id2|||||||", "id3")
  expect_equal(.tt_split_field(x, 1L), c("id1", "id2", "id3"))
  expect_equal(.tt_split_field(x, 2L), c("×", "", ""))
  expect_equal(.tt_split_field(x, 3L), c("Foo", "", ""))
  expect_equal(.tt_split_field(x, 8L), c("L.", "", ""))
})

test_that("tt_matchnames: author-less queries match on canonical name", {
  ref <- c(
    "r1||Foo||bart|||Smith",
    "r2||Foo||barn|||Jones",
    "r3||Foo||zzzzzz|||Smith"
  )
  # Misspelled, author-less: author differences must not count
  res <- tt_matchnames("q1||Foo||bar|||", ref, max_dist = 1)
  expect_equal(res$match_type, c("auto_fuzzy_canon", "auto_fuzzy_canon"))
  expect_equal(res$id_ref, c("r1", "r2"))
  # Exact canonical name wins over looser hits and all of them are returned
  ref2 <- c(ref, "r4||Foo||bar|||Brown", "r5||Foo||bar|||Green")
  res2 <- tt_matchnames("q1||Foo||bar|||", ref2, max_dist = 3)
  expect_equal(res2$match_type, c("auto_cfonly", "auto_cfonly"))
  expect_equal(res2$id_ref, c("r4", "r5"))
  # With an author, matching is still on the whole name (author counts)
  res3 <- tt_matchnames("q1||Foo||bar|||Smyth", ref2, max_dist = 1)
  expect_equal(res3$match_type, "no_match")
})

test_that("tt_matchnames: plain Levenshtein distance, no trailing penalty", {
  # The reference is the query plus extra trailing text, so the distance is
  # exactly max_dist. taxon-tools' gawk amatch() added +1 here and rejected it.
  res <- tt_matchnames("q1||Foo||bar|||Sm", "r1||Foo||bar|||Smit", max_dist = 2)
  expect_equal(res$match_type, "auto_fuzzy")
  res2 <- tt_matchnames("q1||Foo||bar|||Sm", "r1||Foo||bar|||Smith", max_dist = 3)
  expect_equal(res2$match_type, "auto_fuzzy")
  # One below the distance is still a non-match
  res3 <- tt_matchnames("q1||Foo||bar|||Sm", "r1||Foo||bar|||Smith", max_dist = 2)
  expect_equal(res3$match_type, "no_match")
})

test_that("tt_matchnames: canon_closest keeps only the nearest canonical hits", {
  ref <- c("r1||Foo||bart|||A", "r2||Foo||barnn|||B", "r3||Foo||bar t|||C")
  all_hits <- tt_matchnames("q1||Foo||bar|||", ref, max_dist = 3)
  expect_setequal(all_hits$id_ref, c("r1", "r2", "r3"))
  closest <- tt_matchnames("q1||Foo||bar|||", ref, max_dist = 3, canon_closest = TRUE)
  # r1 ("bart") and r3 ("bar t" -> "bart" once depunctuated) are both at
  # distance 1; r2 is at distance 2
  expect_setequal(closest$id_ref, c("r1", "r3"))
  expect_true(all(closest$match_type == "auto_fuzzy_canon"))
})

test_that(".tt_osa counts an adjacent swap as one edit", {
  osa <- function(a, b) .tt_osa(strsplit(a, "")[[1]], b)
  expect_equal(osa("qurecus", "quercus"), 1L)
  expect_equal(osa("abc", "axc"), 1L)
  expect_equal(osa("abcd", "badc"), 2L)
  expect_equal(osa("abc", "abc"), 0L)
  expect_equal(osa("ab", "xyz"), 3L)
})

test_that("tt_matchnames: dist column and swap-aware distance", {
  ref <- c("r1||Foo||quercus|||L.", "r2||Foo||quercxx|||L.")
  q <- "q1||Foo||qurecus|||L."
  swap <- tt_matchnames(q, ref, max_dist = 1, transpositions = TRUE)
  expect_equal(swap$match_type, "auto_fuzzy")
  expect_equal(swap$id_ref, "r1")
  expect_equal(swap$dist, 1L)
  # plain Levenshtein (the default): a swap is two edits
  plain <- tt_matchnames(q, ref, max_dist = 1)
  expect_equal(plain$match_type, "no_match")
  plain2 <- tt_matchnames(q, ref, max_dist = 2)
  expect_equal(plain2$dist, 2L)
  # exact matches have no distance
  expect_true(is.na(tt_matchnames("q||Foo||quercus|||L.", ref)$dist))
})

test_that("tt_matchnames: dist is set for author-less and genus fuzzy hits", {
  ref <- c("r1||Foo||quercus|||L.", "r2||Barus||zzz|||L.")
  canon <- tt_matchnames(
    "q1||Foo||qurecus|||", ref, max_dist = 2, transpositions = TRUE
  )
  expect_equal(canon$match_type, "auto_fuzzy_canon")
  expect_equal(canon$dist, 1L)
  genus <- tt_matchnames(
    "q1||Fo||quercus|||L.", ref, max_dist = 3, max_dist_genus = 2
  )
  expect_equal(genus$match_type, "auto_fuzzy_genus")
  expect_equal(genus$dist, 1L)
})

test_that(".tt_orthfold folds Latin spelling variants", {
  expect_equal(
    .tt_orthfold(c("sylvestris", "silvestris", "caeruleus", "coeruleus", "ceruleus")),
    c("silvestris", "silvestris", "ceruleus", "ceruleus", "ceruleus")
  )
  expect_equal(.tt_orthfold(c("thalii", "rhodo", "phyllum")), c("tali", "rodo", "fillum"))
  # only a final ii is folded
  expect_equal(.tt_orthfold("iinus"), "iinus")
})

test_that("tt_matchnames: auto_orthvar for Latin spelling variants", {
  ref <- c("r1||Foo||sylvestris|||L.", "r2||Foo||caeruleus|||Sm.")
  # with the same author
  res <- tt_matchnames("q1||Foo||silvestris|||L.", ref)
  expect_equal(res$match_type, "auto_orthvar")
  expect_equal(res$id_ref, "r1")
  expect_true(is.na(res$dist))
  # without an author (canonical name)
  res <- tt_matchnames("q1||Foo||coeruleus|||", ref)
  expect_equal(res$match_type, "auto_orthvar")
  expect_equal(res$id_ref, "r2")
  # identical names stay exact; spelling variants never override exact
  res <- tt_matchnames(c("q1||Foo||sylvestris|||L.", "q2||Foo||silvestris|||L."),
    c(ref, "r3||Foo||silvestris|||L."))
  expect_equal(res$match_type, c("exact", "exact"))
  expect_equal(res$id_ref, c("r1", "r3"))
})

test_that("tt_matchnames: orthvar also applies to infraspecific epithets", {
  ref <- "r1||Foo||bar|var.|sylvestris|L."
  res <- tt_matchnames("q1||Foo||bar|var.|silvestris|L.", ref)
  expect_equal(res$match_type, "auto_orthvar")
})
