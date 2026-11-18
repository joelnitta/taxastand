# Pin the current output of behaviors inherited from the taxon-tools engines
# (audit: #166). Each test states what happens today, so a later change to one
# of them shows up as a deliberate edit to a test. Where today's output looks
# wrong, the test says so; fixing it should flip the expectation.
# Follow-up issues: 170 (author particles), 171 (diacritics), 172 (slash),
# 173 (NULL).

.parse_one <- function(x, ...) {
  suppressWarnings(ts_parse_names(x, quiet = TRUE, ...))
}

# Parser fix-ups --------------------------------------------------------------

test_that("'auct.' and 'auctt.' end up in the author, not the infraspecific epithet", {
  res <- .parse_one(c(
    "Foo bar auct.", "Foo bar auctt.", "Foo bar auct. non L.",
    "Foo bar var. auct."
  ))
  expect_identical(
    res$author, c("auct.", "auctt.", "auct. non L.", "auct.")
  )
  expect_true(all(is.na(res$infraspecific_epithet)))
})

test_that("a lone 'd' after the epithet goes to the author", {
  res <- .parse_one(c("Foo bar d", "Foo bar var. d"))
  expect_identical(res$author, c("d", "d"))
  expect_true(all(is.na(res$infraspecific_epithet)))
  # Today the space after 'd' is lost: "d Smith" becomes "dSmith". Looks wrong.
  expect_identical(.parse_one("Foo bar d Smith")$author, "dSmith")
})

test_that("odd characters are removed or replaced before parsing", {
  expect_identical(.parse_one("Foo bar L.¿")$author, "L.") # inverted ?
  expect_identical(.parse_one("Foo bar \"Smith\"")$author, "Smith") # quotes
  expect_identical(.parse_one("Foo bar Smith—Jones")$author, "Smith-Jones")
  expect_identical(.parse_one("Foo bar Smith")$author, "Smith") # nbsp
  # Today "/" is deleted, not replaced by a space: two authors are fused.
  expect_identical(.parse_one("Foo bar Smith/Jones")$author, "SmithJones")
})

test_that("the ligature fl and e-diaeresis are accepted in names", {
  expect_identical(.parse_one("Foo barﬂex")$specific_epithet, "barﬂex")
  expect_identical(.parse_one("Foëgenus bar")$genus_name, "Foëgenus")
})

# Field cleaning in the matcher ------------------------------------------------

test_that("an author written 'NULL' is erased before matching", {
  # Legacy: the matcher blanks any field equal to "NULL" or "\\N" (SQL-dump
  # nulls in the original's input files). A real value is lost.
  f <- .tt_clean_fields("1||Foo||bar|||NULL")
  expect_identical(f$a, "")
  f <- .tt_clean_fields("1||Foo||bar|||\\N")
  expect_identical(f$a, "")
  # Only an exact match is blanked
  f <- .tt_clean_fields("1||Foo||bar|||NULLA")
  expect_identical(f$a, "NULLA")
})

# Diacritics --------------------------------------------------------------------

test_that("tt_depunct flattens the listed Latin-1 letters", {
  expect_identical(
    tt_depunct(c("Müller", "Gómez", "Ångström", "Ørsted")),
    c("muller", "gomez", "angstrom", "orsted")
  )
})

test_that("tt_depunct drops letters outside its table (Latin Extended-A)", {
  # Today these letters vanish instead of being flattened, so "Dvořák"
  # becomes "dvoak" and does not match "Dvorak". Looks wrong: flatten them.
  expect_identical(
    tt_depunct(c("Šmarda", "Dvořák", "Łukasz")),
    c("marda", "dvoak", "ukasz")
  )
})

test_that("stray spaces in a user-supplied parsed data frame are cleaned", {
  # Without .tt_cleanspaces() this would only be an "auto_punct" match.
  ref <- "Foo bar (Smith) Lee"
  query <- ts_parse_names(ref)
  query$author <- "(Smith)   Lee "
  query$genus_name <- "Foo "
  res <- suppressMessages(ts_match_names(query, ref))
  expect_identical(res$match_type, "exact")
})
