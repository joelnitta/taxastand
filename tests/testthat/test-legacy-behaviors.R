# Pin the current output of behaviors inherited from the taxon-tools engines
# (audit: #166). Each test states what happens today, so a later change to one
# of them shows up as a deliberate edit to a test. Where today's output looks
# wrong, the test says so; fixing it should flip the expectation.
# Follow-up issue: 170 (author particles).

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
  # KNOWN WRONG (#170): the space after 'd' is lost, so "d Smith" becomes
  # "dSmith". The author should be "d Smith". Flip this when #170 is fixed.
  expect_identical(.parse_one("Foo bar d Smith")$author, "dSmith")
})

test_that("odd characters are removed or replaced before parsing", {
  expect_identical(.parse_one("Foo bar L.¿")$author, "L.") # inverted ?
  expect_identical(.parse_one("Foo bar \"Smith\"")$author, "Smith") # quotes
  expect_identical(.parse_one("Foo bar Smith—Jones")$author, "Smith-Jones")
  expect_identical(.parse_one("Foo bar Smith")$author, "Smith") # nbsp
  # A slash between authors becomes a space (#172; it used to be deleted,
  # which fused the authors into "SmithJones")
  expect_identical(.parse_one("Foo bar Smith/Jones")$author, "Smith Jones")
})

test_that("the ligature fl and e-diaeresis are accepted in names", {
  expect_identical(.parse_one("Foo barﬂex")$specific_epithet, "barﬂex")
  expect_identical(.parse_one("Foëgenus bar")$genus_name, "Foëgenus")
})

# Field cleaning in the matcher ------------------------------------------------

test_that("an author written 'NULL' or '\\N' is kept", {
  # The original blanked these strings (SQL-dump nulls); that erased a real
  # author "NULL" (#173), so they are now ordinary values.
  f <- .tt_clean_fields("1||Foo||bar|||NULL")
  expect_identical(f$a, "NULL")
  f <- .tt_clean_fields("1||Foo||bar|||\\N")
  expect_identical(f$a, "\\N")
  # An author "NULL" is not the same as no author
  res <- suppressMessages(ts_match_names("Foo bar NULL", "Foo bar"))
  expect_false(res$match_type %in% c("exact", "auto_punct"))
})

# Diacritics --------------------------------------------------------------------

test_that("tt_depunct flattens the listed Latin-1 letters", {
  expect_identical(
    tt_depunct(c("Müller", "Gómez", "Ångström", "Ørsted")),
    c("muller", "gomez", "angstrom", "orsted")
  )
})

test_that("tt_depunct flattens Latin Extended-A letters (#171)", {
  expect_identical(
    tt_depunct(c("\u0160marda", "Dvo\u0159\u00e1k", "\u0141ukasz", "\u0131nce")),
    c("smarda", "dvorak", "lukasz", "ince")
  )
  # Letters that become two letters
  expect_identical(
    tt_depunct(c("Stra\u00dfe", "\u00c6gir", "\u0152uvre")),
    c("strasse", "aegir", "oeuvre")
  )
})

test_that("tt_depunct keeps every letter found in IPNI author names (#171)", {
  # The non-ASCII letters in the standard forms of all 65,409 IPNI author names
  # (kewr::search_ipni(), 2026-11): none may be deleted. Combining marks, the
  # non-breaking space and typographic hyphens/apostrophes are not letters.
  codes <- c(
    0xE1, 0xE9, 0xED, 0xFC, 0xF6, 0xF3, 0xF1, 0xE4, 0x10D, 0x107, 0x161, 0xE7,
    0xE8, 0x131, 0x160, 0xF8, 0xFA, 0x142, 0xFD, 0xE3, 0x11F, 0xC1, 0xD6,
    0x159, 0x144, 0xF4, 0x17E, 0x15F, 0x11B, 0xC7, 0xC9, 0xE2, 0x15E, 0x10C,
    0xEB, 0xEA, 0x15B, 0x103, 0xE5, 0xC5, 0x219, 0xE0, 0x158, 0x21B, 0xF5,
    0x119, 0x16F, 0xF2, 0x1CE, 0x105, 0x117, 0x130, 0x148, 0xD8, 0x15A, 0xEE,
    0x141, 0x17D, 0x12D, 0xEF, 0xE6, 0xEC, 0xD3, 0x151, 0xD0, 0xD4, 0x163,
    0xF9, 0x17C, 0x17B, 0xC0, 0x110, 0x1EC5, 0x21A, 0xDA, 0xDC, 0x1EA7, 0x101,
    0x106, 0x10E, 0x268, 0x136, 0x146, 0x14F, 0x1D1, 0x14D, 0x155, 0xDF,
    0x165, 0x162, 0xFB, 0x16B, 0xFF, 0x17A, 0x179, 0x43E, 0x421, 0xF0
  )
  letters <- intToUtf8(codes, multiple = TRUE)
  expect_true(all(grepl("^[a-z]+$", tt_depunct(letters))))
})

test_that("names that differ only in accents match as punctuation (#171)", {
  res <- suppressMessages(ts_match_names(
    c("Foo bar Dvorak", "Foo bar Smarda"),
    c("Foo bar Dvo\u0159\u00e1k", "Foo bar \u0160marda")
  ))
  expect_identical(res$match_type, c("auto_punct", "auto_punct"))
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
