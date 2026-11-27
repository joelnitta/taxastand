# Pure-R reimplementation of taxon-tools `matchnames` (the `-F` / ALLFUZZY,
# non-interactive mode, which is the only mode taxastand uses).
#
# Copyright (c) Cam Webb, 2018 (taxon-tools, GPL-3), and Joel Nitta, 2026.
# This file is part of taxastand, licensed under GPL-3 (see LICENSE.md).
#
# Originally ported from the gawk script `matchnames` in taxon-tools v1.3.0
# (camwebb/taxon-tools, upstream commit 8f8b5e2). Strict fidelity to it is
# NOT a goal: taxastand aims to be
# accurate, fast, and flexible, so this code may diverge from the original.
# The interactive `-f`
# matching, the `-m` manual-choices file, and stdin prompting are intentionally
# omitted (out of scope; never invoked by taxastand). Variable names below
# (A, B, Bmap, etc.) still mirror the gawk source's array names.
# nolint start: object_name_linter.
#
# Input is a data frame of parsed names, as made by ts_parse_names(): the
# columns id, genus_hybrid_sign, genus_name, species_hybrid_sign,
# specific_epithet, infraspecific_rank, infraspecific_epithet and author.
# Output is the 18-field result format documented in ts_match_names().
#
# Fuzzy matching uses base adist() (Levenshtein distance); a pair is a fuzzy
# match when the distance is <= max_dist.

# Collapse internal/leading/trailing spaces (matchnames `cleanspaces`). Needed
# because `query` and `ref` can be parsed data frames edited by the user, whose
# fields may contain stray spaces (test-legacy-behaviors.R).
.tt_cleanspaces <- function(x) {
  gsub("^ +| +$", "", gsub(" {2,}", " ", x, perl = TRUE), perl = TRUE)
}

# Fold the standard Latin orthographic variants of an epithet to one spelling
# (ICN Art. 60-61 variants): ae/oe -> e, y -> i, ph -> f, rh -> r, th -> t and
# a final ii -> i. `x` must already be depunctuated (lower-case ASCII), so e.g.
# "sylvestris" and "silvestris" both become "silvestris".
.tt_orthfold <- function(x) {
  x <- gsub("ae|oe", "e", x, perl = TRUE)
  x <- gsub("ii$", "i", x, perl = TRUE)
  x <- gsub("y", "i", x, fixed = TRUE)
  x <- gsub("ph", "f", x, fixed = TRUE)
  x <- gsub("rh", "r", x, fixed = TRUE)
  gsub("th", "t", x, fixed = TRUE)
}

# Take the fields matchnames needs from a data frame of parsed names and clean
# them (read_data lines 154-158): cleanspaces; missing values become "". The
# original also blanked the strings NULL and \N (SQL-dump nulls); that is not
# done here, as a real value such as an author "NULL" must be kept (#173).
.tt_clean_fields <- function(parsed) {
  assertthat::assert_that(
    inherits(parsed, "data.frame"),
    msg = "df must be of class 'data.frame'"
  )
  needed <- c("id", .ts_name_parts)
  assertthat::assert_that(
    isTRUE(all(needed %in% colnames(parsed))),
    msg = paste0(
      "df must include the following columns: ",
      toString(needed)
    )
  )
  field <- function(x) {
    x <- as.character(x)
    x[is.na(x)] <- ""
    .tt_cleanspaces(enc2utf8(x))
  }
  list(
    code = enc2utf8(as.character(parsed$id)),
    xg = field(parsed$genus_hybrid_sign), g = field(parsed$genus_name),
    xs = field(parsed$species_hybrid_sign), s = field(parsed$specific_epithet),
    st = field(parsed$infraspecific_rank),
    ssp = field(parsed$infraspecific_epithet), a = field(parsed$author)
  )
}

# The orthographic-variant keys of the records whose epithets contain a letter
# group that .tt_orthfold() changes; all other records have the same key as
# `out`, their punct / canon key
.tt_orth_key <- function(out, f, method) {
  i <- grep("ae|oe|y|ph|rh|th|ii", tolower(paste(f$s, f$ssp)), perl = TRUE)
  if (method == "orthvar") {
    out[i] <- paste0(
      tt_depunct(paste0(f$xg, f$g, f$xs))[i],
      .tt_orthfold(tt_depunct(f$s[i])),
      tt_depunct(f$st[i]),
      .tt_orthfold(tt_depunct(f$ssp[i])),
      tt_depunct(f$a[i])
    )
  } else {
    out[i] <- paste0(
      tt_depunct(f$g[i]),
      .tt_orthfold(tt_depunct(f$s[i])),
      .tt_orthfold(tt_depunct(f$ssp[i]))
    )
  }
  out
}

# Take the fields matchnames needs from a data frame of parsed names.
# Returns a list of equal-length vectors plus `key(method)`, which computes
# (and memoizes) the comparison key for a match method only when first asked
# for, so a method no query ever reaches costs nothing.
.tt_record_keys <- function(parsed) {
  f <- .tt_clean_fields(parsed)
  a <- f$a
  namex <- paste0(f$xg, f$g, f$xs, f$s, f$st, f$ssp) # all parts, no author
  exact <- .tt_cleanspaces(paste(f$xg, f$g, f$xs, f$s, f$st, f$ssp, a))
  punct_in <- paste0(namex, a)
  cache <- new.env(parent = emptyenv())

  # tt_depunct() is a pure function of its input string, so a key that differs
  # from the punct key only for some records is derived from it, re-depuncting
  # just the records whose input string changed.
  derive <- function(new_in) {
    out <- key("punct")
    chg <- new_in != punct_in
    out[chg] <- tt_depunct(new_in[chg])
    out
  }

  key <- function(method) {
    if (!is.null(cache[[method]])) {
      return(cache[[method]])
    }
    val <- switch(method,
      exact = exact,
      punct = tt_depunct(punct_in),
      canon = tt_depunct(paste0(f$g, f$s, f$ssp)),
      irank = derive(paste0(f$xg, f$g, f$xs, f$s, f$ssp, a)),
      basio = derive(paste0(namex, sub("^\\([^)]+\\)", "", a))),
      `in` = derive(paste0(namex, sub(" [iI]n .*$", "", a))),
      # the author after "ex" (who validly published the name); a basionym
      # author in parentheses is kept
      exauth = derive(
        paste0(namex, sub("^(\\([^)]+\\) ?)?(.* )?[eE]x ", "\\1", a))
      ),
      exin = derive(paste0(namex, sub(" ([eE]x|[iI]n) .*$", "", a))),
      # gsub(), not sub(): a name can have both a basionym author and an
      # "ex"/"in" citation, and both must go
      basexin = derive(
        paste0(namex, gsub("(^\\([^)]+\\)| ([eE]x|[iI]n) .*$)", "", a))
      ),
      orthvar = .tt_orth_key(key("punct"), f, method),
      canonorth = .tt_orth_key(key("canon"), f, method),
      stop("unknown key method: ", method)
    )
    assign(method, val, envir = cache)
    val
  }

  list(
    code = f$code, genus = f$g,
    fields = f[c("xg", "g", "xs", "s", "st", "ssp", "a")],
    no_auth = (a == ""), auct = grepl("^auct\\.?$", a),
    key = key
  )
}

# Name of each parsed-name field, in record order, as used in the output
# columns (suffixed `_query` / `_ref`)
.tt_field_suffixes <- c(
  "genus_hybrid_sign", "genus_name", "species_hybrid_sign", "specific_epithet",
  "infraspecific_rank", "infraspecific_epithet", "author"
)

# Optimal string alignment distance (Levenshtein plus swaps of two adjacent
# letters) between the characters `a` (a character vector) and the string `b`.
# Row by row; within a row the insertion step, d[j] = min(t[j], d[j - 1] + 1),
# is a running minimum of t[j] - j, which keeps each row vectorised.
.tt_osa <- function(a, b) {
  b <- strsplit(b, "", fixed = TRUE)[[1]]
  n <- length(a)
  m <- length(b)
  if (n == 0L || m == 0L) {
    return(max(n, m))
  }
  j <- seq_len(m)
  idx <- 0:m
  prev2 <- NULL
  prev <- idx
  for (i in seq_len(n)) {
    x <- prev[j + 1L] + 1L
    y <- prev[j] + (a[i] != b)
    t <- x
    lo <- y < x
    t[lo] <- y[lo]
    if (i > 1L && m > 1L) {
      swap <- which(a[i] == b[-m] & a[i - 1L] == b[-1L])
      if (length(swap)) {
        # swap of a[i - 1], a[i] against b[j - 1], b[j], at columns j = swap + 1
        z <- prev2[swap] + 1L
        at <- swap + 1L
        t[at] <- z - (z > t[at]) * (z - t[at])
      }
    }
    cur <- cummin(c(i, t) - idx) + idx
    prev2 <- prev
    prev <- cur
  }
  prev[m + 1L]
}

#' Match taxonomic names
#'
#' Non-interactive all-fuzzy matching of parsed query names against parsed
#' reference names.
#'
#' @param query,ref Data frames of parsed names (see `ts_parse_names()`).
#' @param ref_keys Optional output of `.tt_record_keys()` for the reference
#'   (e.g. memoized by a prepared reference); used instead of `ref`.
#' @param max_dist Integer; max Levenshtein distance for fuzzy matching.
#' @param match_canon Logical; the `-c` option (CFO / canonical-form match).
#' @param max_dist_genus Integer; max Levenshtein distance between the query
#'   genus and a reference genus for genus-level fuzzy matching, used only for
#'   queries whose genus is not in the reference. 0 disables it.
#' @param transpositions Logical; count a swap of two adjacent letters as one
#'   edit (optimal string alignment distance) instead of two (Levenshtein).
#' @return Data frame of character columns, one row per match (a query with
#'   several fuzzy hits gets several rows; an unmatched query gets one row with
#'   `match_type` "no_match"), with the 19 columns documented in
#'   `ts_match_names()` (`id_query`, `id_ref`, `match_type`, `dist` (integer
#'   edit distance of fuzzy hits, else `NA`), `match_side`, then the seven parsed name fields
#'   suffixed `_query` and `_ref`). Empty values are `NA`.
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils adist
#' @noRd
tt_matchnames <- function(query, ref, max_dist = 3,
                          match_canon = FALSE,
                          max_dist_genus = 0,
                          transpositions = FALSE, ref_keys = NULL) {
  m <- .tt_new_match(
    A = .tt_record_keys(query),
    B = if (is.null(ref_keys)) .tt_record_keys(ref) else ref_keys,
    max_dist = as.numeric(max_dist),
    max_dist_genus = as.numeric(max_dist_genus),
    transpositions = transpositions
  )
  # Exact and rule-based stages, most certain first
  .tt_stage_eq(m, "exact", "exact")
  .tt_stage_eq(m, "punct", "punct")
  .tt_stage_pm(m, "basio", "basionym")
  .tt_stage_pm(m, "in", "in_author")
  .tt_stage_pm(m, "exauth", "ex_validating")
  .tt_stage_eq(m, "exauth", "ex_validating", "both")
  .tt_stage_pm(m, "exin", "ex_first")
  .tt_stage_eq(m, "basexin", "basionym_in_ex", "both")
  .tt_stage_eq(m, "irank", "rank")
  .tt_stage_eq(m, "orthvar", "orthvar")

  .tt_stages_canon_fuzzy(m, match_canon)

  .tt_assemble(m)
}

# The stages after the rule-based ones: canonical-name matches (if allowed) and
# fuzzy matches, for what is still unmatched
.tt_stages_canon_fuzzy <- function(m, match_canon) {
  A <- m$A
  B <- m$B
  # auct.: only a canonical-form match is allowed, else no_match
  cfo <- isTRUE(match_canon)
  auct <- m$pending[A$auct[m$pending]]
  if (cfo && length(auct)) {
    .tt_accept(
      m, auct, .tt_lookup(m, A$key("canon")[auct], "canon"),
      "canonical"
    )
  }

  # Fuzzy matching (only if the genus is present in the reference): queries
  # without an author on canonical name, others on the whole name
  others <- m$pending[!A$auct[m$pending]]
  others <- others[A$genus[others] %in% B$genus]
  canon <- others[A$no_auth[others]]
  if (length(canon)) .tt_stage_canonfuzzy(m, canon)
  whole <- others[!A$no_auth[others]]
  if (length(whole)) .tt_stage_allfuzzy(m, whole)

  # Optional canonical-form match for anything still unmatched
  others <- m$pending[!A$auct[m$pending]]
  if (cfo && length(others)) {
    .tt_accept(
      m, others, .tt_lookup(m, A$key("canon")[others], "canon"),
      "canonical"
    )
  }

  # Genus-level fuzzy matching for queries whose genus is not in the reference
  others <- m$pending[!A$auct[m$pending]]
  others <- others[!A$genus[others] %in% B$genus]
  if (m$max_dist_genus > 0 && length(others)) .tt_stage_genusfuzzy(m, others)
}

# ---- Matching state and stages -----------------------------------------------
# The state lives in an environment `m` so the stage functions can update the
# per-query results without `<<-`: `res_code` (list of matched ref codes),
# `res_type`, `res_side` (which name had the element the rule ignored: "query",
# "reference", "both", or NA), `res_dist` (list of fuzzy distances) and
# `pending` (queries not yet matched).
.tt_new_match <- function(A, B, max_dist, max_dist_genus, transpositions) {
  m <- new.env(parent = emptyenv())
  m$A <- A
  m$B <- B
  m$max_dist <- max_dist
  m$max_dist_genus <- max_dist_genus
  m$transpositions <- transpositions
  n_q <- length(A$code)
  m$res_code <- vector("list", n_q)
  m$res_type <- rep(NA_character_, n_q)
  m$res_side <- rep(NA_character_, n_q)
  m$res_dist <- vector("list", n_q)
  m$pending <- seq_len(n_q)
  m$genus_rows <- NULL
  m
}

# Ref codes whose `method` key equals each of `qkeys`: a list with one
# character vector per query key (empty if none). Every reference record that
# shares the key is returned, in reference order, so ties are reported rather
# than resolved by an arbitrary choice.
.tt_lookup <- function(m, qkeys, method) {
  # Hash the (few) distinct query keys and probe them with the reference keys,
  # rather than hashing the whole reference
  bk <- m$B$key(method)
  u <- unique(qkeys)
  i <- match(bk, u)
  hit <- which(!is.na(i))
  groups <- split(m$B$code[hit], factor(i[hit], levels = seq_along(u)))
  groups[match(qkeys, u)]
}

# Accept matches (not to the query itself) for queries `idx`; `rc` is a list of
# candidate ref codes per query (see .tt_lookup())
.tt_accept <- function(m, idx, rc, type, side = NA_character_) {
  rc <- Map(
    function(codes, self) unique(codes[codes != self]), rc,
    m$A$code[idx]
  )
  ok <- lengths(rc) > 0L
  if (any(ok)) {
    m$res_code[idx[ok]] <- rc[ok]
    m$res_type[idx[ok]] <- type
    m$res_side[idx[ok]] <- side
    m$pending <- setdiff(m$pending, idx[ok])
  }
}

# Record several (fuzzy) hits `codes` for query `qi`, with distances `dist`.
# The caller drops the matched queries from `pending` once it has gone through
# all its queries (.tt_sync_pending()), which is much cheaper than doing it for
# every query.
.tt_record_hits <- function(m, qi, codes, type, dist = NULL) {
  m$res_code[[qi]] <- codes
  m$res_type[qi] <- type
  if (!is.null(dist)) m$res_dist[[qi]] <- dist
}

# Drop queries that have a result from `pending`
.tt_sync_pending <- function(m) {
  m$pending <- m$pending[is.na(m$res_type[m$pending])]
}

# Stage: query[method] key present in the reference
.tt_stage_eq <- function(m, method, outtype, side = NA_character_) {
  idx <- m$pending
  if (length(idx)) {
    .tt_accept(
      m, idx, .tt_lookup(m, m$A$key(method)[idx], method), outtype, side
    )
  }
}

# Stage: the `method` key of one name equals the punct key of the other, i.e.
# one name has an element that the rule ignores. The side says which: the
# reference (the `method` key of the reference is compared with the punct key
# of the query) or the query.
.tt_stage_pm <- function(m, method, outtype) {
  idx <- m$pending
  if (!length(idx)) {
    return(invisible())
  }
  .tt_accept(
    m, idx, .tt_lookup(m, m$A$key("punct")[idx], method),
    outtype, "reference"
  )
  idx <- m$pending
  if (length(idx)) {
    .tt_accept(
      m, idx, .tt_lookup(m, m$A$key(method)[idx], "punct"),
      outtype, "query"
    )
  }
}

# genus -> reference rows (one per distinct code), in reference order (so hit
# order does not depend on the arbitrary id strings or on which reference names
# were filtered out); built on first use
.tt_genus_rows <- function(m) {
  if (is.null(m$genus_rows)) {
    first <- !duplicated(m$B$code)
    m$genus_rows <- split(which(first), m$B$genus[first])
  }
  m$genus_rows
}

# Edit distance between query key `q` and reference keys `r`: Levenshtein
# (adist), then, if `transpositions`, lowered by one per swap of two adjacent
# letters (optimal string alignment). A swap needs a swapped bigram of `q` to
# occur in `r`; with k such bigrams, OSA >= Levenshtein - k. The exact (slow)
# calculation therefore only runs on pairs where a swap could matter: a
# swapped bigram is present, and OSA could be within max_dist.
#
# Keys whose length differs from `q` by more than max_dist cannot be within
# max_dist edits (each edit, a swap included, changes the length by at most
# one), so they are not compared and their distance is left `NA`; every
# distance that is <= max_dist is unchanged.
.tt_fuzzy_dist <- function(m, q, r) {
  d <- rep(NA_integer_, length(r))
  near <- which(abs(nchar(r) - nchar(q)) <= m$max_dist)
  d[near] <- as.integer(adist(q, r[near]))
  if (!m$transpositions || is.na(q) || nchar(q) < 2L) {
    return(d)
  }
  ch <- strsplit(q, "", fixed = TRUE)[[1]]
  n <- length(ch)
  swapped <- unique(paste0(ch[-1], ch[-n])[ch[-1] != ch[-n]])
  todo <- which(!is.na(d) & d >= 2L & d <= 2 * m$max_dist)
  if (length(swapped) && length(todo)) {
    rt <- r[todo]
    k <- 0L
    for (bg in swapped) k <- k + grepl(bg, rt, fixed = TRUE)
    todo <- todo[k >= 1L & d[todo] - k <= m$max_dist]
    for (i in todo) d[i] <- .tt_osa(ch, r[i])
  }
  d
}

# Reference rows (not the query itself) among `rows`
.tt_not_self <- function(m, rows, qi) {
  rows[m$B$code[rows] != m$A$code[qi]]
}

# Stage: a query without an author is compared with the reference on canonical
# name (genus + epithet + infraspecific epithet) so that the author string does
# not count towards the distance. All same-genus refs with an identical
# canonical name are returned ("canonical"); failing that, those whose
# canonical name differs only by Latin spelling variants ("orthvar");
# failing that, all within max_dist ("fuzzy_canonical").
.tt_stage_canonfuzzy <- function(m, idx) {
  rcf <- m$B$key("canon")
  qcf <- m$A$key("canon")
  orth <- NULL # canonical orthvar keys, built when first needed
  for (qi in idx) {
    rows <- .tt_not_self(m, .tt_genus_rows(m)[[m$A$genus[qi]]], qi)
    if (!length(rows)) next
    d <- .tt_fuzzy_dist(m, qcf[qi], rcf[rows])
    if (any(d == 0L, na.rm = TRUE)) {
      keep <- !is.na(d) & d == 0L
      type <- "canonical"
    } else {
      if (is.null(orth)) {
        orth <- list(q = m$A$key("canonorth"), r = m$B$key("canonorth"))
      }
      same <- !is.na(orth$r[rows]) & orth$r[rows] == orth$q[qi]
      if (any(same)) {
        keep <- same
        type <- "orthvar"
      } else {
        keep <- !is.na(d) & d <= m$max_dist
        type <- "fuzzy_canonical"
      }
    }
    if (any(keep)) {
      .tt_record_hits(
        m, qi, m$B$code[rows][keep], type,
        if (type == "fuzzy_canonical") d[keep]
      )
    }
  }
  .tt_sync_pending(m)
}

# Stage: every same-genus ref within max_dist of the whole (depunctuated) name
.tt_stage_allfuzzy <- function(m, idx) {
  rpunct <- m$B$key("punct")
  qpunct <- m$A$key("punct")
  for (qi in idx) {
    rows <- .tt_not_self(m, .tt_genus_rows(m)[[m$A$genus[qi]]], qi)
    if (!length(rows)) next
    d <- .tt_fuzzy_dist(m, qpunct[qi], rpunct[rows])
    keep <- !is.na(d) & d <= m$max_dist
    if (any(keep)) {
      .tt_record_hits(m, qi, m$B$code[rows][keep], "fuzzy", d[keep])
    }
  }
  .tt_sync_pending(m)
}

# Stage: the query genus is not in the reference (typically a misspelling).
# Candidates are the names in reference genera within `max_dist_genus` of the
# query genus; they are then compared with the query as in the other fuzzy
# stages (author-less queries on the canonical name, others on the whole name),
# with the total distance, genus edits included, limited by `max_dist`. Match
# type "fuzzy_genus".
.tt_stage_genusfuzzy <- function(m, idx) {
  ref_genera <- unique(m$B$genus)
  ref_genera_key <- tt_depunct(ref_genera)
  keys <- list(
    canon = list(q = m$A$key("canon"), r = m$B$key("canon")),
    whole = list(q = m$A$key("punct"), r = m$B$key("punct"))
  )
  near <- list()
  for (qi in idx) {
    qg <- m$A$genus[qi]
    if (is.null(near[[qg]])) {
      d <- as.integer(adist(tt_depunct(qg), ref_genera_key))
      near[[qg]] <- ref_genera[!is.na(d) & d <= m$max_dist_genus]
    }
    rows <- unique(unlist(.tt_genus_rows(m)[near[[qg]]], use.names = FALSE))
    rows <- .tt_not_self(m, rows, qi)
    if (!length(rows)) next
    k <- keys[[if (m$A$no_auth[qi]) "canon" else "whole"]]
    d <- .tt_fuzzy_dist(m, k$q[qi], k$r[rows])
    keep <- !is.na(d) & d <= m$max_dist
    if (any(keep)) {
      .tt_record_hits(m, qi, m$B$code[rows][keep], "fuzzy_genus", d[keep])
    }
  }
  .tt_sync_pending(m)
}

# Combine the per-query results into the output data frame (one row per match;
# unmatched queries get one "no_match" row)
.tt_assemble <- function(m) {
  A <- m$A
  B <- m$B
  n_q <- length(A$code)
  m$res_code[m$pending] <- list(NA_character_)
  m$res_type[m$pending] <- "no_match"

  n_hits <- lengths(m$res_code)
  qi <- rep(seq_len(n_q), n_hits)
  rc <- unlist(m$res_code, use.names = FALSE)
  type <- rep(m$res_type, n_hits)
  side <- rep(m$res_side, n_hits)
  dist <- unlist(
    lapply(seq_len(n_q), function(i) {
      if (is.null(m$res_dist[[i]])) {
        rep(NA_integer_, n_hits[i])
      } else {
        m$res_dist[[i]]
      }
    }),
    use.names = FALSE
  )
  if (is.null(dist)) dist <- integer(0)

  # Ref row for each matched code. If a code is duplicated in the reference,
  # the last record wins.
  ri <- length(B$code) + 1L - match(rc, rev(B$code))

  na_empty <- function(x) {
    x[!is.na(x) & x == ""] <- NA_character_
    x
  }
  out <- list(
    id_query = A$code[qi],
    id_ref = rc,
    match_type = type,
    dist = dist,
    match_side = side
  )
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_query")]] <- A$fields[[k]][qi]
  }
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_ref")]] <- B$fields[[k]][ri]
  }
  out[-4] <- lapply(out[-4], na_empty)
  as.data.frame(out)
}
# nolint end
