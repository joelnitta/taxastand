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
  expect_equal(ncol(res), 17)
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
