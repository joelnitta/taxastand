# Pure-R reimplementation of taxon-tools `parsenames`
#
# Faithful port of the gawk script `parsenames` from taxon-tools v1.3.0
# (camwebb/taxon-tools, upstream commit 8f8b5e2). The goal is byte-for-byte
# identical output to the original tool. Non-ASCII characters in the original
# regexes are written here as \u escapes so this source stays ASCII/portable;
# see the inline comments for the literal character each escape represents.
#
# The main parsing regex is applied with perl = TRUE: PCRE's greedy
# left-to-right matching reproduces gawk's capture-group assignment for this
# (intentionally ambiguous) pattern, whereas R's TRE engine assigns the groups
# differently. The "(*UCP)" verb keeps POSIX classes such as [:alnum:]
# Unicode-aware (so accented author names are captured, as under gawk's locale).
# Input is normalised to UTF-8 first.

# Character classes reused below, written to avoid backslash-in-bracket
# ambiguity between gawk and TRE (hyphen placed last = literal hyphen):
#   \u00d7 = MULTIPLICATION SIGN (hybrid mark), \u00eb = e-diaeresis,
#   \ufb02 = LATIN SMALL LIGATURE FL, \u2019 = RIGHT SINGLE QUOTATION MARK

# The single large parsing regex (parsenames line 61). Groups:
#   1 genus hybrid | 2 genus | 3 species hybrid | 4 species |
#   5 infraspecific rank | 6 infraspecific epithet | 7 author string
.tt_parse_re <- paste0(
  "(*UCP)", # Unicode-aware POSIX classes
  "^([\u00d7xX]?) ?", # 1 genus hybrid sign
  "([A-Z][a-z\u00eb-]+) ?", # 2 genus name
  "([\u00d7xX]? |[\u00d7X]?) ?", # 3 species hybrid sign
  "([a-z\ufb02-][a-z\ufb02-]+)? ?", # 4 specific epithet
  "(var\\.|f\\.|forma|subf\\.|taxon|fo\\.|subsp\\.|prol\\.|nothovar\\.|lus\\.|\\[infrasp\\.unranked\\])? ?", # 5 rank
  "([a-z\ufb02_-]+)? ?", # 6 infraspecific epithet
  # 7 author: ']' placed first (literal), '-' last (literal); includes
  # [:alnum:] plus the combining diacritics used in author names
  "([] [().&;,\u2019'[:alnum:]\u0301 \u0300 \u0308 \u0306 \u030c \u0327 \u0326 \u0303 \u030a \u0302 -]+)?$"
)

# Per-field validation regexes (parsenames lines 94-99)
.tt_v1 <- "^[\u00d7xX]?$"
.tt_v2 <- "^[A-Z][a-z\u00eb-]+$"
.tt_v3 <- "^[\u00d7xX]?$"
.tt_v4 <- "^[a-z\ufb02-][a-z\ufb02-]+$"
.tt_v5 <- "^([a-z]+\\.?|\\[infrasp\\.unranked\\])?$"
.tt_v6 <- "^([a-z\ufb02-][a-z\ufb02_-]+)?$"

#' Remove punctuation and flatten diacritics (taxon-tools `depunct`)
#'
#' Faithful port of the `depunct()` gawk function shared by parsenames and
#' matchnames. Vectorised over `x`.
#'
#' @param x Character vector.
#' @return Character vector, depunctuated and lower-cased.
#' @keywords internal
#' @noRd
tt_depunct <- function(x) {
  # Diacritic flattening (precomposed Latin-1 / Latin Extended letters)
  x <- gsub("[\u00f9\u00fa\u00fb\u00fc]", "u", x) # \u00f9\u00fa\u00fb\u00fc
  x <- gsub("[\u00d1]", "N", x) # \u00d1
  x <- gsub("[\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5]", "A", x) # \u00c0\u00c1\u00c2\u00c3\u00c4\u00c5
  x <- gsub("[\u00ec\u00ed\u00ee\u00ef]", "i", x) # \u00ec\u00ed\u00ee\u00ef
  x <- gsub("[\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8]", "O", x) # \u00d2\u00d3\u00d4\u00d5\u00d6\u00d8
  x <- gsub("[\u00c7]", "C", x) # \u00c7
  x <- gsub("[\u00e6]", "ae", x) # \u00e6
  x <- gsub("[\u00d0]", "D", x) # \u00d0
  x <- gsub("[\u00fd\u00ff]", "y", x) # \u00fd\u00ff
  x <- gsub("[\u00c8\u00c9\u00ca\u00cb]", "E", x) # \u00c8\u00c9\u00ca\u00cb
  x <- gsub("[\u00f1]", "n", x) # \u00f1
  x <- gsub("[\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5]", "a", x) # \u00e0\u00e1\u00e2\u00e3\u00e4\u00e5
  x <- gsub("[\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8]", "o", x) # \u00f2\u00f3\u00f4\u00f5\u00f6\u00f8
  x <- gsub("[\u00df]", "b", x) # \u00df
  x <- gsub("[\u00d9\u00da\u00db\u00dc]", "U", x) # \u00d9\u00da\u00db\u00dc
  x <- gsub("[\u00de\u00fe]", "p", x) # \u00de\u00fe
  x <- gsub("[\u00e7\u010d]", "c", x) # \u00e7\u010d
  x <- gsub("[\u00cc\u00cd\u00ce\u00cf]", "I", x) # \u00cc\u00cd\u00ce\u00cf
  x <- gsub("[\u00f0]", "d", x) # \u00f0
  x <- gsub("[\u00e8\u00e9\u00ea\u00eb]", "e", x) # \u00e8\u00e9\u00ea\u00eb
  x <- gsub("[\u00c6]", "Ae", x) # \u00c6
  x <- gsub("[\u00dd]", "Y", x) # \u00dd

  # Parentheses to underscore (later stripped); ' and / et ' to ' & '
  x <- gsub("[()]", "_", x)
  x <- gsub(" (and|et.?)  ", " \\& ", x)

  # Keep only letters, digits, &, and the hybrid sign; then lower-case
  x <- gsub("[^A-Za-z0-9()&\u00d7]", "", x)
  tolower(x)
}

# Helper: extract field i (1-based) from a string with exactly 6 pipes.
.tt_field <- function(x, i) {
  if (i == 1) {
    sub("^([^|]*)\\|.*$", "\\1", x)
  } else {
    sub(
      paste0("^([^|]*\\|){", i - 1, "}([^|]*)\\|.*$"),
      "\\2", x
    )
  }
}

#' Parse taxonomic names (taxon-tools `parsenames`)
#'
#' Faithful pure-R port. Takes a character vector of `id|name` records and
#' returns a character vector of `id|g_hybrid|genus|s_hybrid|species|rank|
#' infrasp|author` records. Names that fail validation are returned as `id|`
#' (all parsed fields empty), exactly as the original tool emits them.
#'
#' @param records Character vector of `id|name` lines.
#' @return Character vector of parsed `|`-delimited lines.
#' @keywords internal
#' @noRd
tt_parsenames <- function(records) {
  if (length(records) == 0) {
    return(character(0))
  }

  records <- enc2utf8(records)
  id <- sub("\\|.*$", "", records)
  name <- sub("^[^|]*\\|", "", records)

  # Main-block preprocessing (parsenames lines 14-18)
  name <- gsub("  *", " ", name)
  name <- gsub("( *$|^ *)", "", name)
  name <- gsub(" ssp\\. ", " subsp. ", name)

  # parse_taxon_name: clean bad chars (lines 54-56)
  name <- gsub("[\u00bf/\"]", "", name) # remove \u00bf / "
  name <- gsub("\u00a0", " ", name) # non-breaking space -> space
  name <- gsub("\u2014", "-", name) # em dash -> hyphen

  # The big anchored parse regex (line 61); perl = TRUE to match gawk captures
  parsed <- sub(.tt_parse_re, "\\1|\\2|\\3|\\4|\\5|\\6|\\7", name, perl = TRUE)

  # Fix-ups (lines 65, 73-78)
  parsed <- gsub(" *\\|", "|", parsed)
  parsed <- gsub("\\|(auctt?)\\|\\.", "||\\1.", parsed)
  parsed <- gsub("\\|d\\|", "||d", parsed)
  parsed <- gsub("\\|[xX]\\|", "|\u00d7|", parsed)
  parsed <- gsub("^[xX]\\|", "\u00d7|", parsed)

  # Validation (test = 1; lines 87-101). A name fails if it produced no pipe,
  # any field regex fails, or the depuncted re-made string differs from input.
  matched <- grepl("|", parsed, fixed = TRUE)

  remade <- gsub("\\|", " ", parsed)
  remade <- gsub("  +", " ", remade)
  remade <- gsub("^ ", "", remade)
  remade <- gsub(" $", "", remade)

  p1 <- .tt_field(parsed, 1)
  p2 <- .tt_field(parsed, 2)
  p3 <- .tt_field(parsed, 3)
  p4 <- .tt_field(parsed, 4)
  p5 <- .tt_field(parsed, 5)
  p6 <- .tt_field(parsed, 6)

  depunct_eq <-
    gsub("\u00d7", "x", tt_depunct(remade)) ==
      gsub("\u00d7", "x", tt_depunct(name))

  fail <-
    !matched |
      !grepl(.tt_v1, p1, perl = TRUE) |
      !grepl(.tt_v2, p2, perl = TRUE) |
      !grepl(.tt_v3, p3, perl = TRUE) |
      !grepl(.tt_v4, p4, perl = TRUE) |
      !grepl(.tt_v5, p5, perl = TRUE) |
      !grepl(.tt_v6, p6, perl = TRUE) |
      !depunct_eq

  # On failure the original returns "" -> output line is just `id|`
  out_parsed <- ifelse(fail, "", parsed)
  paste0(id, "|", out_parsed)
}
