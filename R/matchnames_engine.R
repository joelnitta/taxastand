# Pure-R reimplementation of taxon-tools `matchnames` (the `-F` / ALLFUZZY,
# non-interactive mode, which is the only mode taxastand uses).
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
# Input is the parsed pipe-delimited format produced by tt_parsenames /
# ts_write_names: id|g_hybrid|genus|s_hybrid|species|rank|infrasp|author # nolint: commented_code_linter.
# Output is the 18-field result format documented in ts_match_names().
#
# Fuzzy matching uses base adist() (Levenshtein distance); a pair is a fuzzy
# match when the distance is <= max_dist.

# Collapse internal/leading/trailing spaces (matchnames `cleanspaces`)
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

# Parse a vector of parsed `id|...` lines into the fields matchnames needs.
# Returns a list of equal-length vectors plus `key(method)`, which computes
# (and memoises) the comparison key for a match method only when first asked
# for, so a method no query ever reaches costs nothing.
.tt_record_keys <- function(lines) {
  lines <- enc2utf8(lines)
  # Split into 8 fields (exactly 7 pipes); fields may be empty.
  fields <- .tt_split_fields(lines, 8L)
  f <- function(i) {
    if (i == 8L) {
      # the author keeps everything after the 7th pipe
      sub("^([^|]*\\|){7}(.*)$", "\\2", lines, perl = TRUE)
    } else {
      fields[[i]]
    }
  }
  code <- f(1)

  # Field cleaning (read_data lines 154-158): blank out NULL / \N, cleanspaces
  clean <- function(x) {
    x[x == "NULL" | x == "\\N"] <- ""
    .tt_cleanspaces(x)
  }
  xg <- clean(f(2))
  g <- clean(f(3))
  xs <- clean(f(4))
  s <- clean(f(5))
  st <- clean(f(6))
  ssp <- clean(f(7))
  a <- clean(f(8))

  namex <- paste0(xg, g, xs, s, st, ssp) # all parts, no author
  exact <- .tt_cleanspaces(paste(xg, g, xs, s, st, ssp, a))
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

  # Records whose epithets contain a letter group that .tt_orthfold() changes;
  # all other records have the same orthvar key as their punct / cfonly key
  orth_trigger <- function() {
    which(grepl("ae|oe|y|ph|rh|th|ii", tolower(paste(s, ssp)), perl = TRUE))
  }

  key <- function(method) {
    if (!is.null(cache[[method]])) {
      return(cache[[method]])
    }
    val <- switch(method,
      exact = exact,
      punct = tt_depunct(punct_in),
      noauth = tt_depunct(namex),
      cfonly = tt_depunct(paste0(g, s, ssp)),
      irank = derive(paste0(xg, g, xs, s, ssp, a)),
      basio = derive(paste0(namex, sub("^\\([^)]+\\)", "", a))),
      `in` = derive(paste0(namex, sub(" [iI]n .*$", "", a))),
      exin = derive(paste0(namex, sub(" ([eE]x|[iI]n) .*$", "", a))),
      basexin = derive(
        paste0(namex, sub("(^\\([^)]+\\)| ([eE]x|[iI]n) .*$)", "", a))
      ),
      orthvar = {
        out <- key("punct")
        i <- orth_trigger()
        out[i] <- paste0(
          tt_depunct(paste0(xg, g, xs))[i],
          .tt_orthfold(tt_depunct(s[i])),
          tt_depunct(st[i]),
          .tt_orthfold(tt_depunct(ssp[i])),
          tt_depunct(a[i])
        )
        out
      },
      cforth = {
        out <- key("cfonly")
        i <- orth_trigger()
        out[i] <- paste0(
          tt_depunct(g[i]),
          .tt_orthfold(tt_depunct(s[i])),
          .tt_orthfold(tt_depunct(ssp[i]))
        )
        out
      },
      stop("unknown key method: ", method)
    )
    assign(method, val, envir = cache)
    val
  }

  list(
    code = code, genus = g,
    fields = list(xg = xg, g = g, xs = xs, s = s, st = st, ssp = ssp, a = a),
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
#' @param query_lines,ref_lines Character vectors of parsed `id|...` lines.
#' @param max_dist Integer; max Levenshtein distance for fuzzy matching.
#' @param match_no_auth Logical; the `-1` option (NOAUTHONE).
#' @param match_canon Logical; the `-c` option (CFO / canonical-form match).
#' @param canon_closest Logical; for fuzzy canonical-name matches of queries
#'   without an author, return only the hits at the smallest distance (ties
#'   kept) instead of all hits within `max_dist`.
#' @param max_dist_genus Integer; max Levenshtein distance between the query
#'   genus and a reference genus for genus-level fuzzy matching, used only for
#'   queries whose genus is not in the reference. 0 disables it.
#' @param transpositions Logical; count a swap of two adjacent letters as one
#'   edit (optimal string alignment distance) instead of two (Levenshtein).
#' @return Data frame of character columns, one row per match (a query with
#'   several fuzzy hits gets several rows; an unmatched query gets one row with
#'   `match_type` "no_match"), with the 18 columns documented in `ts_match_names()` (`id_query`, `id_ref`,
#'   `match_type`, `dist` (integer edit distance of fuzzy hits, else `NA`), then the seven parsed name fields suffixed `_query` and
#'   `_ref`). Empty values are `NA`.
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils adist
#' @noRd
tt_matchnames <- function(query_lines, ref_lines, max_dist = 3,
                          match_no_auth = FALSE, match_canon = FALSE,
                          canon_closest = FALSE, max_dist_genus = 0,
                          transpositions = FALSE) {
  max_dist <- as.numeric(max_dist)
  max_dist_genus <- as.numeric(max_dist_genus)
  noauthone <- isTRUE(match_no_auth)
  cfo <- isTRUE(match_canon)

  B <- .tt_record_keys(ref_lines)
  A <- .tt_record_keys(query_lines)
  n_q <- length(A$code)

  # Per query: matched ref code(s) and match type, filled in as tests succeed.
  # `pending` holds the queries not yet matched.
  res_code <- vector("list", n_q)
  res_type <- rep(NA_character_, n_q)
  res_dist <- vector("list", n_q)
  pending <- seq_len(n_q)

  # Ref code whose `method` key equals each of `qkeys` (NA if none). If a key
  # occurs more than once in the reference, the last record wins.
  lookup <- function(qkeys, method) {
    bk <- B$key(method)
    B$code[length(bk) + 1L - match(qkeys, rev(bk))]
  }

  # Accept matches (not to the query itself) for queries `idx`
  accept <- function(idx, rc, type) {
    ok <- !is.na(rc) & rc != A$code[idx]
    if (any(ok)) {
      res_code[idx[ok]] <<- as.list(rc[ok])
      res_type[idx[ok]] <<- type
      pending <<- setdiff(pending, idx[ok])
    }
  }

  # test_eq: query[method] key present in B[method]
  test_eq <- function(method, outtype) {
    idx <- pending
    if (length(idx)) accept(idx, lookup(A$key(method)[idx], method), outtype)
  }

  # test_na: noauth key unique in B
  test_na <- function() {
    idx <- pending[A$no_auth[pending]]
    if (!length(idx)) {
      return(invisible())
    }
    bk <- B$key("noauth")
    u <- unique(bk)
    counts <- tabulate(match(bk, u), length(u))
    qk <- A$key("noauth")[idx]
    rc <- lookup(qk, "noauth")
    rc[is.na(counts[match(qk, u)]) | counts[match(qk, u)] != 1L] <- NA
    accept(idx, rc, "auto_noauth")
  }

  # test_pm: plus/minus variants using punct on the opposite side
  test_pm <- function(method) {
    idx <- pending
    if (!length(idx)) {
      return(invisible())
    }
    accept(
      idx, lookup(A$key("punct")[idx], method), paste0("auto_", method, "-")
    )
    idx <- pending
    if (length(idx)) {
      accept(
        idx, lookup(A$key(method)[idx], "punct"), paste0("auto_", method, "+")
      )
    }
  }

  # genus -> reference rows (one per distinct code), in reference order (so hit
  # order does not depend on the arbitrary id strings or on which reference
  # names were filtered out); built on first use
  genus_cache <- new.env(parent = emptyenv())
  genus_rows <- function() {
    if (!exists("rows", envir = genus_cache, inherits = FALSE)) {
      first <- !duplicated(B$code)
      assign(
        "rows", split(which(first), B$genus[first]),
        envir = genus_cache
      )
    }
    get("rows", envir = genus_cache, inherits = FALSE)
  }

  # Edit distance between query key `q` and reference keys `r`: Levenshtein
  # (adist), then, if `transpositions`, lowered by one per swap of two adjacent
  # letters (optimal string alignment). A swap needs a swapped bigram of `q` to
  # occur in `r`; with k such bigrams, OSA >= Levenshtein - k. The exact (slow)
  # calculation therefore only runs on pairs where a swap could matter: a
  # swapped bigram is present, and OSA could be within max_dist.
  fuzzy_dist <- function(q, r) {
    d <- as.integer(adist(q, r))
    if (!transpositions || is.na(q) || nchar(q) < 2L) {
      return(d)
    }
    ch <- strsplit(q, "", fixed = TRUE)[[1]]
    n <- length(ch)
    swapped <- unique(paste0(ch[-1], ch[-n])[ch[-1] != ch[-n]])
    todo <- which(!is.na(d) & d >= 2L & d <= 2 * max_dist)
    if (length(swapped) && length(todo)) {
      rt <- r[todo]
      k <- 0L
      for (bg in swapped) k <- k + grepl(bg, rt, fixed = TRUE)
      todo <- todo[k >= 1L & d[todo] - k <= max_dist]
      for (i in todo) d[i] <- .tt_osa(ch, r[i])
    }
    d
  }

  # test_canonfuzzy: a query without an author is compared with the reference
  # on canonical name (genus + epithet + infraspecific epithet) so that the
  # author string does not count towards the distance. All same-genus refs with
  # an identical canonical name are returned ("auto_cfonly"); failing that, all
  # within max_dist ("auto_fuzzy_canon"), or only the closest of those if
  # `canon_closest`.
  test_canonfuzzy <- function(idx) {
    rcf <- B$key("cfonly")
    qcf <- A$key("cfonly")
    orth <- NULL # canonical orthvar keys, built when first needed
    for (qi in idx) {
      rows <- genus_rows()[[A$genus[qi]]]
      rows <- rows[B$code[rows] != A$code[qi]]
      if (!length(rows)) next
      cand <- B$code[rows]
      d <- fuzzy_dist(qcf[qi], rcf[rows])
      if (any(d == 0L, na.rm = TRUE)) {
        keep <- !is.na(d) & d == 0L
        type <- "auto_cfonly"
      } else {
        if (is.null(orth)) {
          orth <- list(q = A$key("cforth"), r = B$key("cforth"))
        }
        same <- !is.na(orth$r[rows]) & orth$r[rows] == orth$q[qi]
        if (any(same)) {
          keep <- same
          type <- "auto_orthvar"
        } else {
          keep <- !is.na(d) & d <= max_dist
          if (canon_closest && any(keep)) keep <- keep & d == min(d[keep])
          type <- "auto_fuzzy_canon"
        }
      }
      if (any(keep)) {
        res_code[[qi]] <<- cand[keep]
        res_type[qi] <<- type
        if (type == "auto_fuzzy_canon") res_dist[[qi]] <<- d[keep]
        pending <<- setdiff(pending, qi)
      }
    }
  }

  # test_allfuzzy: every same-genus ref within max_dist
  test_allfuzzy <- function(idx) {
    rpunct <- B$key("punct")
    qpunct <- A$key("punct")
    for (qi in idx) {
      rows <- genus_rows()[[A$genus[qi]]]
      rows <- rows[B$code[rows] != A$code[qi]]
      if (!length(rows)) next
      cand <- B$code[rows]
      qp <- qpunct[qi]
      rp <- rpunct[rows]
      # Distance on the depunctuated names
      d <- fuzzy_dist(qp, rp)
      keep <- !is.na(d) & d <= max_dist
      if (any(keep)) {
        res_code[[qi]] <<- cand[keep]
        res_type[qi] <<- "auto_fuzzy"
        res_dist[[qi]] <<- d[keep]
        pending <<- setdiff(pending, qi)
      }
    }
  }

  # test_genusfuzzy: the query genus is not in the reference (typically a
  # misspelling). Candidates are the names in reference genera within
  # `max_dist_genus` of the query genus; they are then compared with the query
  # as in the other fuzzy tests (author-less queries on the canonical name,
  # others on the whole name), with the total distance, genus edits included,
  # limited by `max_dist`. Match type "auto_fuzzy_genus".
  test_genusfuzzy <- function(idx) {
    ref_genera <- unique(B$genus)
    ref_genera_key <- tt_depunct(ref_genera)
    keys <- list(
      canon = list(q = A$key("cfonly"), r = B$key("cfonly")),
      whole = list(q = A$key("punct"), r = B$key("punct"))
    )
    near <- list()
    for (qi in idx) {
      qg <- A$genus[qi]
      if (is.null(near[[qg]])) {
        d <- as.integer(adist(tt_depunct(qg), ref_genera_key))
        near[[qg]] <- ref_genera[!is.na(d) & d <= max_dist_genus]
      }
      rows <- unique(unlist(genus_rows()[near[[qg]]], use.names = FALSE))
      rows <- rows[B$code[rows] != A$code[qi]]
      if (!length(rows)) next
      cand <- B$code[rows]
      k <- keys[[if (A$no_auth[qi]) "canon" else "whole"]]
      d <- fuzzy_dist(k$q[qi], k$r[rows])
      keep <- !is.na(d) & d <= max_dist
      if (canon_closest && A$no_auth[qi] && any(keep)) {
        keep <- keep & d == min(d[keep])
      }
      if (any(keep)) {
        res_code[[qi]] <<- cand[keep]
        res_type[qi] <<- "auto_fuzzy_genus"
        res_dist[[qi]] <<- d[keep]
        pending <<- setdiff(pending, qi)
      }
    }
  }

  test_eq("exact", "exact")
  test_eq("punct", "auto_punct")
  if (noauthone) test_na()
  test_pm("basio")
  test_pm("in")
  test_pm("exin")
  test_eq("basexin", "auto_basexin")
  test_eq("irank", "auto_irank")
  test_eq("orthvar", "auto_orthvar")

  # auct.: only a canonical-form match is allowed, else no_match
  auct <- pending[A$auct[pending]]
  if (cfo && length(auct)) {
    accept(auct, lookup(A$key("cfonly")[auct], "cfonly"), "auto_cfonly")
  }

  # Fuzzy matching (only if the genus is present in the reference): queries
  # without an author on canonical name, others on the whole name
  others <- pending[!A$auct[pending]]
  others <- others[A$genus[others] %in% B$genus]
  canon <- others[A$no_auth[others]]
  if (length(canon)) test_canonfuzzy(canon)
  whole <- others[!A$no_auth[others]]
  if (length(whole)) test_allfuzzy(whole)

  # Optional canonical-form match for anything still unmatched
  others <- pending[!A$auct[pending]]
  if (cfo && length(others)) {
    accept(others, lookup(A$key("cfonly")[others], "cfonly"), "auto_cfonly")
  }

  # Genus-level fuzzy matching for queries whose genus is not in the reference
  others <- pending[!A$auct[pending]]
  others <- others[!A$genus[others] %in% B$genus]
  if (max_dist_genus > 0 && length(others)) test_genusfuzzy(others)

  res_code[pending] <- list(NA_character_)
  res_type[pending] <- "no_match"

  n_hits <- lengths(res_code)
  qi <- rep(seq_len(n_q), n_hits)
  rc <- unlist(res_code, use.names = FALSE)
  type <- rep(res_type, n_hits)
  dist <- unlist(
    lapply(seq_len(n_q), function(i) {
      if (is.null(res_dist[[i]])) rep(NA_integer_, n_hits[i]) else res_dist[[i]]
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
    dist = dist
  )
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_query")]] <- A$fields[[k]][qi]
  }
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_ref")]] <- B$fields[[k]][ri]
  }
  out[-4] <- lapply(out[-4], na_empty)
  as.data.frame(out, stringsAsFactors = FALSE)
}
# nolint end
