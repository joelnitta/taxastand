# Pure-R reimplementation of taxon-tools `parsenames`
#
# Copyright (c) Cam Webb, 2018 (taxon-tools, GPL-3), and Joel Nitta, 2026.
# This file is part of taxastand, licensed under GPL-3 (see LICENSE.md).
#
# Originally ported from the gawk script `parsenames` in taxon-tools v1.3.0
# (camwebb/taxon-tools, upstream commit 8f8b5e2). Strict fidelity to it is
# NOT a goal: taxastand aims to be
# accurate, fast, and flexible, so this code may diverge from the original.
# Non-ASCII characters in the original
# regexes are written here as \u escapes so this source stays ASCII/portable;
# see the inline comments for the literal character each escape represents.
#
# The main parsing regex is applied with perl = TRUE: PCRE's greedy
# left-to-right matching reproduces gawk's capture-group assignment for this
# (intentionally ambiguous) pattern, whereas R's TRE engine assigns the groups
# differently. The "(*UCP)" verb keeps POSIX classes such as [:alnum:]
# Unicode-aware (so accented author names are captured, as under gawk's locale).
# Input is normalized to UTF-8 first.

# The regex accepts the ligature \ufb02 in epithets and e-diaeresis in genus
# names, as the original did (no reason is recorded upstream); both parse
# correctly and are pinned in test-legacy-behaviors.R.
#
# Character classes reused below, written to avoid backslash-in-bracket
# ambiguity between gawk and TRE (hyphen placed last = literal hyphen):
#   \u00d7 = MULTIPLICATION SIGN (hybrid mark), \u00eb = e-diaeresis,
#   \ufb02 = LATIN SMALL LIGATURE FL, \u2019 = RIGHT SINGLE QUOTATION MARK

# Words that begin an author string although they are lowercase (#170). IPNI
# author standard forms (65,409 authors, 2026-11) begin with de, van, d', del,
# den, du, la, da, ten, von, di, ter or hort.; but queries need not come from
# IPNI, so the list also has the other common particles of European names
# (Romance, Germanic, Dutch, Scandinavian), and "der"/"den" for chains such as
# "van der Werff". "auct." (auctorum) is added, and the citation words "ex",
# "sensu", "non" and "nec", which also begin the author in some names (`ex
# DC.`, `sensu Pruski`; about 180 WFO names). They are nomenclatural, so they
# are kept in the author (#174). Without it the infraspecific
# epithet group below would take the word as an epithet, so it is limited to a
# closed list: a lowercase word counts as a particle only when a capital letter
# or "(" follows it, with or without a space ("duQuesnay"), or when nothing
# follows it (such a name is then rejected: see .tt_parse_failed()).
.tt_particles <- paste(
  c(
    "de", "del", "della", "dell", "dei", "degli", "di", "da", "das", "dos",
    "do", "du", "des", "van", "von", "vom", "der", "den", "ter", "ten", "te",
    "la", "las", "le", "les", "lo", "los", "el", "af", "zu", "zur",
    # citation words that begin an author: "ex DC.", "sensu Smith", "non L.",
    # "in Piper" (IPNI writes the author this way when the first is unknown)
    "ex", "in", "sensu", "non", "nec"
  ),
  collapse = "|"
)
.tt_particle_chain <- paste0("(?:", .tt_particles, ")(?: (?:", .tt_particles, "))*")
.tt_author_particles <- paste0(
  "(?:", .tt_particle_chain, "(?: (?:[[:upper:]]|\\()|(?=[[:upper:]])|$)",
  "|d['\u2019]|d (?=[[:upper:]])|(?:auctt?|hort)\\.)"
)

# The single large parsing regex (parsenames line 61). Groups:
#   1 genus hybrid | 2 genus | 3 species hybrid | 4 species |
#   5 infraspecific rank | 6 infraspecific epithet | 7 author string
.tt_parse_re <- paste0(
  "(*UCP)", # Unicode-aware POSIX classes
  "^([\u00d7xX]?) ?", # 1 genus hybrid sign
  "([A-Z][a-z\u00eb-]+) ?", # 2 genus name
  "([\u00d7xX]? |[\u00d7X]?) ?", # 3 species hybrid sign
  "([a-z\ufb02-][a-z\ufb02-]+)? ?", # 4 specific epithet
  # 5 rank
  "(var\\.|f\\.|forma|subf\\.|taxon|fo\\.|subsp\\.|prol\\.|nothovar\\.|",
  "lus\\.|\\[infrasp\\.unranked\\])? ?",
  "(?!", .tt_author_particles, ")([a-z\ufb02_-]+)? ?", # 6 infraspecific epithet
  # 7 author: ']' placed first (literal), '-' last (literal); includes
  # [:alnum:] plus the combining diacritics used in author names
  "([] [().&;,\u2019'[:alnum:]\u0301 \u0300 \u0308 \u0306 \u030c ",
  "\u0327 \u0326 \u0303 \u030a \u0302 -]+)?$"
)

# Per-field validation regexes (parsenames lines 94-99)
.tt_v1 <- "^[\u00d7xX]?$"
.tt_v2 <- "^[A-Z][a-z\u00eb-]+$"
.tt_v3 <- "^[\u00d7xX]?$"
.tt_v4 <- "^[a-z\ufb02-][a-z\ufb02-]+$"
.tt_v5 <- "^([a-z]+\\.?|\\[infrasp\\.unranked\\])?$"
.tt_v6 <- "^([a-z\ufb02-][a-z\ufb02_-]+)?$"

# Open-nomenclature qualifier tokens (not part of taxon-tools; taxastand
# extension, opt-in via `strip_qualifiers`). Matches whole tokens only, via
# zero-width lookaround on surrounding whitespace/string boundaries, so
# words that merely contain these substrings (e.g. "affine") are untouched.
# `cf.`/`aff.` are open nomenclature (ON) qualifiers as reviewed by Sigovini,
# Keppel & Tagliapietra (2016) <doi:10.1111/2041-210X.12594>; `s.l.`/`s.str.`
# (sensu lato/stricto) are a separate, older circumscription convention (not
# part of that ON review) included here as a taxastand extension.
.tt_qualifier_re <- paste0(
  "(?<=\\s|^)(",
  "cf\\.?|",
  "aff\\.?|",
  "sensu stricto|",
  "sensu lato|",
  "s\\.str\\.?|",
  "s\\.l\\.?",
  ")(?=\\s|$)"
)

# The letters with diacritics that tt_depunct() flattens: for each plain
# letter, the accented letters that become it. Covers Latin-1, Latin
# Extended-A and every other letter found in the standard forms of the 65,409
# IPNI author names (2026-11), so author names compare without their accents.
# A letter that is not here is deleted by tt_depunct(), not flattened.
.tt_diacritic_letters <- c(
  a = paste0(
    "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105\u01ce",
    "\u1ea7\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u0100\u0102\u0104"
  ),
  c = "\u00e7\u0107\u0109\u010b\u010d\u00c7\u0106\u0108\u010a\u010c",
  d = "\u00f0\u010f\u0111\u00d0\u010e\u0110",
  e = paste0(
    "\u00e8\u00e9\u00ea\u00eb\u0113\u0115\u0117\u0119\u011b\u1ec5",
    "\u00c8\u00c9\u00ca\u00cb\u0112\u0114\u0116\u0118\u011a"
  ),
  g = "\u011d\u011f\u0121\u0123\u011c\u011e\u0120\u0122",
  h = "\u0125\u0127\u0124\u0126",
  i = paste0(
    "\u00ec\u00ed\u00ee\u00ef\u0129\u012b\u012d\u012f\u0131\u0268",
    "\u00cc\u00cd\u00ce\u00cf\u0128\u012a\u012c\u012e\u0130"
  ),
  j = "\u0135\u0134",
  k = "\u0137\u0136",
  l = "\u013a\u013c\u013e\u0140\u0142\u0139\u013b\u013d\u013f\u0141",
  n = "\u00f1\u0144\u0146\u0148\u00d1\u0143\u0145\u0147",
  o = paste0(
    "\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u014f\u0151\u01d1",
    "\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u014c\u014e\u0150\u043e"
  ),
  r = "\u0155\u0157\u0159\u0154\u0156\u0158",
  s = "\u015b\u015d\u015f\u0161\u0219\u015a\u015c\u015e\u0160\u0218\u0421",
  t = "\u0163\u0165\u0167\u021b\u0162\u0164\u0166\u021a",
  u = paste0(
    "\u00f9\u00fa\u00fb\u00fc\u0169\u016b\u016d\u016f\u0171\u0173",
    "\u00d9\u00da\u00db\u00dc\u0168\u016a\u016c\u016e\u0170\u0172"
  ),
  w = "\u0175\u0174",
  y = "\u00fd\u00ff\u0177\u00dd\u0176\u0178",
  z = "\u017a\u017c\u017e\u0179\u017b\u017d"
)

# Letters that become two letters (not a one-for-one swap)
.tt_diacritic_ligatures <- c(
  "\u00e6" = "ae", "\u00c6" = "ae", "\u0153" = "oe", "\u0152" = "oe",
  "\u00df" = "ss", "\u00fe" = "th", "\u00de" = "th",
  "\u0133" = "ij", "\u0132" = "ij"
)

# The one-for-one swaps as a pair of strings for chartr()
.tt_diacritics <- list(
  from = paste(.tt_diacritic_letters, collapse = ""),
  to = paste(
    strrep(names(.tt_diacritic_letters), nchar(.tt_diacritic_letters)),
    collapse = ""
  )
)

#' Remove punctuation and flatten diacritics
#'
#' Used by name parsing and matching. Vectorised over `x`.
#'
#' @param x Character vector.
#' @return Character vector, depunctuated and lower-cased.
#' @keywords internal
#' @noRd
tt_depunct <- function(x) {
  # Diacritic flattening (precomposed Latin-1 / Latin Extended letters). Almost
  # all names are pure ASCII, so only run these passes on the (few) elements
  # that contain non-ASCII characters.
  nonascii <- which(nchar(x, type = "bytes") != nchar(x, type = "chars"))
  if (length(nonascii)) {
    y <- chartr(.tt_diacritics$from, .tt_diacritics$to, x[nonascii])
    for (i in seq_along(.tt_diacritic_ligatures)) {
      y <- gsub(
        names(.tt_diacritic_ligatures)[i], .tt_diacritic_ligatures[[i]], y,
        fixed = TRUE
      )
    }
    x[nonascii] <- y
  }

  # ' and ' / ' et ' (also ' et. ') to ' & '
  x <- gsub(" (and|et)\\.? ", " \\& ", x, perl = TRUE)

  # Keep only letters, digits, &, and the hybrid sign (parentheses and every
  # other character go); then lower-case
  x <- gsub("[^A-Za-z0-9&\u00d7]", "", x, perl = TRUE)
  tolower(x)
}

#' Parse taxonomic names
#'
#' Pure-R port. Takes a character vector of names and returns a data frame with
#' one row per name and the seven parts as character columns: genus hybrid
#' sign, genus, species hybrid sign, specific epithet, infraspecific rank,
#' infraspecific epithet and author. A part that is absent is `NA`. Names that
#' fail validation have all parts `NA`.
#'
#' @param names Character vector of names.
#' @param strip_qualifiers Logical; if `TRUE`, remove standalone
#' open-nomenclature qualifier tokens (`cf.`, `aff.`, `s.l.`/`sensu lato`,
#' `s.str.`/`sensu stricto`) before parsing. Off by default.
#' @return Data frame with the columns in `.ts_name_parts`.
#' @keywords internal
#' @noRd
tt_parsenames <- function(names, strip_qualifiers = FALSE) {
  name <- enc2utf8(names)

  # Optional taxastand extension: strip open-nomenclature qualifiers before
  # the whitespace clean-up below removes the double space left behind.
  if (isTRUE(strip_qualifiers)) {
    name <- gsub(.tt_qualifier_re, "", name, perl = TRUE)
  }

  # A slash separates people in an author string ("Smith/Jones"): make it a
  # space, before the whitespace clean-up below (the original deleted it,
  # which fused the two authors)
  name <- gsub("/", " ", name, fixed = TRUE)

  # Main-block preprocessing (parsenames lines 14-18)
  name <- gsub(" {2,}", " ", name, perl = TRUE)
  name <- gsub("( +$|^ +)", "", name, perl = TRUE)
  name <- gsub(" ssp. ", " subsp. ", name, fixed = TRUE)

  # parse_taxon_name: clean characters that cannot be part of a name (lines
  # 54-56): inverted question mark and double quotes are removed, a
  # non-breaking space becomes a space and an em dash a hyphen (the upstream
  # source links an issue about these in a names-resolver)
  name <- gsub("[\u00bf\"]", "", name, perl = TRUE) # remove \u00bf and "
  name <- gsub("\u00a0", " ", name, fixed = TRUE) # non-breaking space -> space
  name <- gsub("\u2014", "-", name, fixed = TRUE) # em dash -> hyphen

  # The big anchored parse regex (line 61); perl = TRUE to match gawk captures.
  # The seven capture groups are the parts of the name; a group that did not
  # take part in the match, or a name that did not match, gives "".
  m <- regexpr(.tt_parse_re, name, perl = TRUE)
  start <- attr(m, "capture.start")
  len <- attr(m, "capture.length")
  parts <- lapply(seq_len(7L), function(i) {
    ifelse(start[, i] < 1L, "", substr(name, start[, i], start[, i] + len[, i] - 1L))
  })
  matched <- m > 0L

  # Fix-ups (lines 65, 73-78): spaces before a separator are dropped, and a
  # lone x is the hybrid sign
  parts[1:6] <- lapply(parts[1:6], function(x) {
    x <- sub(" +$", "", x, perl = TRUE)
    x[x %in% c("x", "X")] <- "\u00d7"
    x
  })

  fail <- .tt_parse_failed(parts, name, matched)

  # A failed name has all parts empty
  parsed <- lapply(parts, function(x) {
    x[fail | x == ""] <- NA_character_
    x
  })
  names(parsed) <- .ts_name_parts
  as.data.frame(parsed, stringsAsFactors = FALSE)
}

# Validation (test = 1; lines 87-101). A name fails if it did not match, any
# field regex fails, or the depuncted re-made string differs from the input.
# `parts` is a list of the seven fields as character vectors ("" if empty).
.tt_parse_failed <- function(parts, name, matched) {
  remade <- do.call(paste, c(parts, sep = " "))
  remade <- gsub(" {2,}", " ", remade, perl = TRUE)
  remade <- gsub("(^ | $)", "", remade, perl = TRUE)

  depunct_eq <-
    gsub("\u00d7", "x", tt_depunct(remade), fixed = TRUE) ==
      gsub("\u00d7", "x", tt_depunct(name), fixed = TRUE)

  # A particle alone ("Foo bar de") is not an author
  only_particles <- grepl(
    paste0("^", .tt_particle_chain, "$"), parts[[7]],
    perl = TRUE
  )

  !matched |
    only_particles |
    !grepl(.tt_v1, parts[[1]], perl = TRUE) |
    !grepl(.tt_v2, parts[[2]], perl = TRUE) |
    !grepl(.tt_v3, parts[[3]], perl = TRUE) |
    !grepl(.tt_v4, parts[[4]], perl = TRUE) |
    !grepl(.tt_v5, parts[[5]], perl = TRUE) |
    !grepl(.tt_v6, parts[[6]], perl = TRUE) |
    !depunct_eq
}
