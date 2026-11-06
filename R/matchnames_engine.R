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
# Output is the 17-field result format documented in ts_match_names().
#
# Fuzzy matching uses base adist() (Levenshtein); the original uses gawk's
# anchored amatch(), documented in-source as equivalent to
# levenshtein(...) <= FUZZERR. # nolint: commented_code_linter.

# Collapse internal/leading/trailing spaces (matchnames `cleanspaces`)
.tt_cleanspaces <- function(x) {
  x <- gsub("  *", " ", x)
  x <- gsub(" *$", "", x)
  gsub("^ *", "", x)
}

# Parse a vector of parsed `id|...` lines into the fields matchnames needs.
# Returns a list of equal-length vectors plus `key(method)`, which computes
# (and memoises) the comparison key for a match method only when first asked
# for, so a method no query ever reaches costs nothing.
.tt_record_keys <- function(lines) {
  lines <- enc2utf8(lines)
  # Split into 8 fields (exactly 7 pipes); fields may be empty.
  f <- function(i) {
    if (i == 1) {
      sub("^([^|]*)\\|.*$", "\\1", lines)
    } else if (i == 8) {
      sub("^([^|]*\\|){7}(.*)$", "\\2", lines)
    } else {
      sub(paste0("^([^|]*\\|){", i - 1, "}([^|]*)\\|.*$"), "\\2", lines)
    }
  }
  code <- f(1)

  # Field cleaning (read_data lines 154-158): blank out NULL / \N, cleanspaces
  clean <- function(x) {
    x <- sub("^(NULL|\\\\N)$", "", x)
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

# For boundary fuzzy pairs (Levenshtein distance == FUZZERR), decide whether
# gawk's amatch adds its trailing-deletion penalty. The penalty applies when
# matching "^query$" against the reference requires deleting trailing
# character(s) off the end of the reference, i.e. when the deletion-from-ref
# step is the *unique* optimal final step of the alignment (no equally cheap
# match/substitution or insertion ending). Returns an integer 0/1 per ref.
# `full` is the precomputed adist(query, ref) for each ref.
.tt_amatch_trailing <- function(query, refs, full) {
  m <- nchar(query, type = "chars")
  pm <- if (m > 0) substr(query, 1, m - 1) else ""
  qlast <- if (m > 0) substr(query, m, m) else ""
  out <- integer(length(refs))
  for (k in seq_along(refs)) {
    t <- refs[k]
    n <- nchar(t, type = "chars")
    if (n == 0) next
    # deletion of the last ref char must itself be an optimal final step
    del <- as.integer(adist(query, substr(t, 1, n - 1))) + 1L
    if (del != full[k]) next
    tlast <- substr(t, n, n)
    diag <- as.integer(adist(pm, substr(t, 1, n - 1))) +
      (if (m > 0 && qlast == tlast) 0L else 1L)
    up <- as.integer(adist(pm, t)) + 1L
    if (full[k] < diag && full[k] < up) out[k] <- 1L
  }
  out
}

# Name of each parsed-name field, in record order, as used in the output
# columns (suffixed `_query` / `_ref`)
.tt_field_suffixes <- c(
  "genus_hybrid_sign", "genus_name", "species_hybrid_sign", "specific_epithet",
  "infraspecific_rank", "infraspecific_epithet", "author"
)

#' Match taxonomic names
#'
#' Non-interactive all-fuzzy matching of parsed query names against parsed
#' reference names.
#'
#' @param query_lines,ref_lines Character vectors of parsed `id|...` lines.
#' @param max_dist Integer; max Levenshtein distance for fuzzy matching.
#' @param match_no_auth Logical; the `-1` option (NOAUTHONE).
#' @param match_canon Logical; the `-c` option (CFO / canonical-form match).
#' @return Data frame of character columns, one row per match (a query with
#'   several fuzzy hits gets several rows; an unmatched query gets one row with
#'   `match_type` "no_match"), with the 17 columns documented in `ts_match_names()` (`id_query`, `id_ref`,
#'   `match_type`, then the seven parsed name fields suffixed `_query` and
#'   `_ref`). Empty values are `NA`.
#' @keywords internal
#' @importFrom stats setNames
#' @importFrom utils adist
#' @noRd
tt_matchnames <- function(query_lines, ref_lines, max_dist = 10,
                          match_no_auth = FALSE, match_canon = FALSE) {
  fuzzerr <- as.numeric(max_dist)
  noauthone <- isTRUE(match_no_auth)
  cfo <- isTRUE(match_canon)

  B <- .tt_record_keys(ref_lines)
  A <- .tt_record_keys(query_lines)
  n_q <- length(A$code)

  # Per query: matched ref code(s) and match type, filled in as tests succeed.
  # `pending` holds the queries not yet matched.
  res_code <- vector("list", n_q)
  res_type <- rep(NA_character_, n_q)
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

  # test_allfuzzy: every same-genus ref within max_dist
  test_allfuzzy <- function(idx) {
    # genus -> ref codes, in reference order (so hit order does not depend on
    # the arbitrary id strings or on which reference names were filtered out)
    genus_codes <- lapply(split(B$code, B$genus), unique)
    punct_by_code <- setNames(B$key("punct"), B$code)
    qpunct <- A$key("punct")
    for (qi in idx) {
      cand <- genus_codes[[A$genus[qi]]]
      cand <- cand[cand != A$code[qi]]
      if (!length(cand)) next
      qp <- qpunct[qi]
      rp <- punct_by_code[cand]
      # gawk's amatch(text = ref, "^query$", FUZZERR) accepts a pair when its
      # cost <= FUZZERR. Probing aregex directly shows the cost equals the
      # Levenshtein distance, plus exactly 1 when the optimal alignment must
      # delete trailing character(s) off the end of the reference to reach the
      # "$" anchor (a pure trailing append in the reference); all other edit
      # patterns cost the plain distance. Since the penalty is at most 1, it
      # only changes the outcome at the boundary distance == FUZZERR.
      d <- as.integer(adist(qp, rp))
      keep <- !is.na(d) & d < fuzzerr
      bnd <- which(!is.na(d) & d == fuzzerr)
      if (length(bnd)) {
        keep[bnd] <- .tt_amatch_trailing(qp, rp[bnd], d[bnd]) == 0L
      }
      if (any(keep)) {
        res_code[[qi]] <<- cand[keep]
        res_type[qi] <<- "auto_fuzzy"
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

  # auct.: only a canonical-form match is allowed, else no_match
  auct <- pending[A$auct[pending]]
  if (cfo && length(auct)) {
    accept(auct, lookup(A$key("cfonly")[auct], "cfonly"), "auto_cfonly")
  }

  # all-fuzzy (only if genus present in reference), then canonical form
  others <- pending[!A$auct[pending]]
  fuzzy <- others[A$genus[others] %in% B$genus]
  if (length(fuzzy)) test_allfuzzy(fuzzy)
  others <- pending[!A$auct[pending]]
  if (cfo && length(others)) {
    accept(others, lookup(A$key("cfonly")[others], "cfonly"), "auto_cfonly")
  }

  res_code[pending] <- list(NA_character_)
  res_type[pending] <- "no_match"

  n_hits <- lengths(res_code)
  qi <- rep(seq_len(n_q), n_hits)
  rc <- unlist(res_code, use.names = FALSE)
  type <- rep(res_type, n_hits)

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
    match_type = type
  )
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_query")]] <- A$fields[[k]][qi]
  }
  for (k in seq_along(.tt_field_suffixes)) {
    out[[paste0(.tt_field_suffixes[k], "_ref")]] <- B$fields[[k]][ri]
  }
  out <- lapply(out, na_empty)
  as.data.frame(out, stringsAsFactors = FALSE)
}
# nolint end
