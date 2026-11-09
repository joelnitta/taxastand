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

test_that("tt_matchnames: every reference record sharing a key is returned", {
  ref <- c("r1||Foo||bar|||L.", "r2||Foo||bar|||L.")
  res <- tt_matchnames("q1||Foo||bar|||L.", ref)
  expect_equal(res$id_ref, c("r1", "r2"))
  expect_equal(res$match_type, c("exact", "exact"))
  res2 <- tt_matchnames("q1||Foo||bar|||L.", rev(ref))
  expect_setequal(res2$id_ref, c("r1", "r2"))
})

test_that("tt_matchnames: ties at the author/form stages return all records", {
  cases <- list(
    auto_punct = list("q1||Foo||bar|||Willd,", c("r1||Foo||bar|||Willd.", "r2||Foo||bar|||Willd")),
    `auto_exin-` = list("q1||Foo||bar|||Willd.", c("r1||Foo||bar|||Willd. ex Less.", "r2||Foo||bar|||Willd. ex DC.")),
    `auto_in-` = list("q1||Foo||bar|||Willd.", c("r1||Foo||bar|||Willd. in Less.", "r2||Foo||bar|||Willd. in DC.")),
    `auto_basio-` = list("q1||Foo||bar|||Willd.", c("r1||Foo||bar|||(L.) Willd.", "r2||Foo||bar|||(DC.) Willd.")),
    auto_irank = list("q1||Foo||bar|f.|baz|Willd.", c("r1||Foo||bar|var.|baz|Willd.", "r2||Foo||bar|subsp.|baz|Willd."))
  )
  for (type in names(cases)) {
    ref <- cases[[type]][[2]]
    for (r in list(ref, rev(ref))) {
      res <- tt_matchnames(cases[[type]][[1]], r)
      expect_setequal(res$id_ref, c("r1", "r2"))
      expect_true(all(res$match_type == type), info = type)
    }
  }
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

test_that(".tt_split_fields splits parser output, incl. short and empty fields", {
  x <- c("id1|×|Foo|||bar||L.", "id2|||||||", "id3")
  f <- .tt_split_fields(x, 8L)
  expect_equal(f[[1]], c("id1", "id2", "id3"))
  expect_equal(f[[2]], c("×", "", ""))
  expect_equal(f[[3]], c("Foo", "", ""))
  expect_equal(f[[8]], c("L.", "", ""))
  expect_equal(.tt_split_fields(character(0), 3L), rep(list(character(0)), 3))
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
  expect_equal(res2$match_type, c("auto_canon", "auto_canon"))
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

test_that("tt_matchnames: match_no_auth matches a unique author-less name", {
  ref <- c("r1||Foo||bar|||L.", "r2||Foo||baz|||L.")
  q <- "q1||Foo||bar|||"
  expect_equal(tt_matchnames(q, ref)$match_type, "auto_canon")
  res <- tt_matchnames(q, ref, match_no_auth = TRUE)
  expect_equal(res$match_type, "auto_noauth")
  expect_equal(res$id_ref, "r1")
  # not unique in the reference: no auto_noauth, both are canonical hits
  ref2 <- c(ref, "r3||Foo||bar|||Sm.")
  res2 <- tt_matchnames(q, ref2, match_no_auth = TRUE)
  expect_equal(res2$match_type, c("auto_canon", "auto_canon"))
  expect_setequal(res2$id_ref, c("r1", "r3"))
})

test_that("tt_matchnames: match_canon matches on canonical name only", {
  ref <- "r1||Foo||bar|||L."
  far <- "q1||Foo||bar|||Zzzzzzzzzzz."
  expect_equal(tt_matchnames(far, ref)$match_type, "no_match")
  res <- tt_matchnames(far, ref, match_canon = TRUE)
  expect_equal(res$match_type, "auto_canon")
  expect_equal(res$id_ref, "r1")
  # auct. queries are never fuzzy-matched, only canonically (if allowed)
  auct <- "q1||Foo||bar|||auct."
  expect_equal(tt_matchnames(auct, ref)$match_type, "no_match")
  expect_equal(tt_matchnames(auct, ref, match_canon = TRUE)$match_type, "auto_canon")
})

test_that("tt_matchnames: a name never matches itself", {
  rec <- "a||Foo||bar|||L."
  expect_equal(tt_matchnames(rec, rec)$match_type, "no_match")
  expect_equal(tt_matchnames("a||Foo||bar|||", rec)$match_type, "no_match")
})

test_that("tt_matchnames: closest-only applies to genus-level fuzzy hits", {
  ref <- c("r1||Foo||bar|||", "r2||Foo||baxxx|||")
  q <- "q1||Fo||bar|||"
  all <- tt_matchnames(q, ref, max_dist = 4, max_dist_genus = 2)
  expect_equal(nrow(all), 2)
  closest <- tt_matchnames(
    q, ref, max_dist = 4, max_dist_genus = 2, canon_closest = TRUE
  )
  expect_equal(closest$id_ref, "r1")
  expect_equal(closest$match_type, "auto_fuzzy_genus")
})

test_that("tt_matchnames handles no queries; .tt_osa handles empty strings", {
  res <- tt_matchnames(character(0), "r1||Foo||bar|||L.")
  expect_equal(nrow(res), 0)
  expect_equal(ncol(res), 18)
  expect_equal(.tt_osa(character(0), "ab"), 2L)
  expect_equal(.tt_osa(c("a", "b"), ""), 2L)
})

test_that("tt_matchnames: match_no_auth is a no-op for queries with an author", {
  res <- tt_matchnames(
    "q1||Foo||bar|||L.", "r1||Foo||bar|||Sm.", match_no_auth = TRUE
  )
  expect_equal(res$match_type, "auto_fuzzy")
})

test_that("tt_parsenames returns nothing for no records", {
  expect_equal(tt_parsenames(character(0)), character(0))
})

# ---- Speed-ups that must not change results ----------------------------------

test_that("the length bound loses no fuzzy hit", {
  set.seed(1)
  rand <- function(n) {
    vapply(
      seq_len(n),
      function(i) {
        paste(sample(letters[1:4], sample(3:12, 1), replace = TRUE), collapse = "")
      },
      ""
    )
  }
  r <- unique(rand(300))
  for (max_dist in c(0, 1, 2, 3)) {
    m <- new.env()
    m$max_dist <- max_dist
    for (transpositions in c(FALSE, TRUE)) {
      m$transpositions <- transpositions
      for (q in rand(15)) {
        d <- .tt_fuzzy_dist(m, q, r)
        full <- if (transpositions) {
          vapply(r, function(x) .tt_osa(strsplit(q, "")[[1]], x), 0, USE.NAMES = FALSE)
        } else {
          as.integer(adist(q, r))
        }
        expect_equal(which(!is.na(d) & d <= max_dist), which(full <= max_dist))
        hit <- which(full <= max_dist)
        expect_equal(d[hit], as.integer(full[hit]))
      }
    }
  }
})

test_that("a reference key lookup returns all records and handles NA", {
  m <- new.env()
  m$B <- list(
    code = c("a", "b", "c", "d"),
    key = function(method) c("x", "y", "x", NA)
  )
  got <- .tt_lookup(m, c("x", "x", "z", NA, "y"), "exact")
  expect_equal(got, list(c("a", "c"), c("a", "c"), character(0), "d", "b"),
               ignore_attr = TRUE)
  m$B <- list(code = character(), key = function(method) character())
  expect_equal(lengths(.tt_lookup(m, c("x", "y"), "exact")), c(0L, 0L),
               ignore_attr = TRUE)
})
