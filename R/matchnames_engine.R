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

# Compute, for a vector of parsed `id|...` lines, every key/field matchnames
# needs (vectorised). Returns a list of equal-length vectors.
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
  xg <- f(2)
  g <- f(3)
  xs <- f(4)
  s <- f(5)
  st <- f(6)
  ssp <- f(7)
  a <- f(8)

  # Field cleaning (read_data lines 154-158): blank out NULL / \N, cleanspaces
  clean <- function(x) {
    x <- sub("^(NULL|\\\\N)$", "", x)
    .tt_cleanspaces(x)
  }
  xg <- clean(xg)
  g <- clean(g)
  xs <- clean(xs)
  s <- clean(s)
  st <- clean(st)
  ssp <- clean(ssp)
  a <- clean(a)

  exact <- .tt_cleanspaces(paste(xg, g, xs, s, st, ssp, a))

  namex <- paste0(xg, g, xs, s, st, ssp) # all parts, no author
  punct <- tt_depunct(paste0(namex, a))
  noauth <- tt_depunct(namex)
  cfonly <- tt_depunct(paste0(g, s, ssp))
  irank <- tt_depunct(paste0(xg, g, xs, s, ssp, a))
  a_basio <- sub("^\\([^)]+\\)", "", a)
  basio <- tt_depunct(paste0(namex, a_basio))
  a_in <- sub(" [iI]n .*$", "", a)
  in_ <- tt_depunct(paste0(namex, a_in))
  a_exin <- sub(" ([eE]x|[iI]n) .*$", "", a)
  exin <- tt_depunct(paste0(namex, a_exin))
  a_bx <- sub("(^\\([^)]+\\)| ([eE]x|[iI]n) .*$)", "", a)
  basexin <- tt_depunct(paste0(namex, a_bx))

  list(
    code = code, genus = g,
    fields = list(xg = xg, g = g, xs = xs, s = s, st = st, ssp = ssp, a = a),
    no_auth = (a == ""), auct = grepl("^auct\\.?$", a),
    exact = exact, punct = punct, noauth = noauth, cfonly = cfonly,
    irank = irank, basio = basio, `in` = in_, exin = exin, basexin = basexin
  )
}

# Build a hashed key -> code environment (last write wins, file order)
.tt_build_map <- function(keys, codes) {
  e <- new.env(hash = TRUE, size = max(1L, length(keys)))
  for (j in seq_along(keys)) assign(paste0("k:", keys[j]), codes[j], envir = e)
  e
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

# Look up a key; return NA_character_ if absent
.tt_get <- function(e, key) {
  k <- paste0("k:", key)
  if (exists(k, envir = e, inherits = FALSE)) {
    get(k, envir = e, inherits = FALSE)
  } else {
    NA_character_
  }
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

  methods <- c(
    "exact", "punct", "noauth", "cfonly", "irank",
    "basio", "in", "exin", "basexin"
  )
  Bmap <- lapply(methods, function(m) .tt_build_map(B[[m]], B$code))
  names(Bmap) <- methods

  # noauth-key counts (XNA)
  Bxna <- table(B$noauth)

  # Genus -> ascending-sorted ref codes (Gh, iterated in @ind_str_asc order)
  Bgenus_codes <- lapply(split(B$code, B$genus), function(x) sort(unique(x)))
  Bgenus_present <- names(Bgenus_codes)
  # punct keyed by code for fuzzy candidate lookup
  Bpunct_by_code <- setNames(B$punct, B$code)

  # Each test returns NULL (no match) or list(codes = <ref codes>, type = <chr>)
  hit <- function(codes, type) list(codes = codes, type = type)

  # test_eq: query[method] key present in B[method]
  test_eq <- function(qi, method, outtype) {
    rc <- .tt_get(Bmap[[method]], A[[method]][qi])
    if (!is.na(rc) && rc != A$code[qi]) hit(rc, outtype) else NULL
  }
  # test_na: noauth key unique in B
  test_na <- function(qi) {
    key <- A$noauth[qi]
    rc <- .tt_get(Bmap[["noauth"]], key)
    cnt <- Bxna[key]
    if (!is.na(rc) && !is.na(cnt) && cnt == 1 && rc != A$code[qi]) {
      hit(rc, "auto_noauth")
    } else {
      NULL
    }
  }
  # test_pm: plus/minus variants using punct on the opposite side
  test_pm <- function(qi, method) {
    rc <- .tt_get(Bmap[[method]], A$punct[qi])
    if (!is.na(rc) && rc != A$code[qi]) {
      return(hit(rc, paste0("auto_", method, "-")))
    }
    rc <- .tt_get(Bmap[["punct"]], A[[method]][qi])
    if (!is.na(rc) && rc != A$code[qi]) {
      return(hit(rc, paste0("auto_", method, "+")))
    }
    NULL
  }
  # test_allfuzzy: every same-genus ref within max_dist
  test_allfuzzy <- function(qi) {
    cand <- Bgenus_codes[[A$genus[qi]]]
    cand <- cand[cand != A$code[qi]]
    if (!length(cand)) {
      return(NULL)
    }
    qp <- A$punct[qi]
    rp <- Bpunct_by_code[cand]
    # gawk's amatch(text = ref, "^query$", FUZZERR) accepts a pair when its
    # cost <= FUZZERR. Probing aregex directly shows the cost equals the
    # Levenshtein distance, plus exactly 1 when the optimal alignment must
    # delete trailing character(s) off the end of the reference to reach the
    # "$" anchor (a pure trailing append in the reference); all other edit
    # patterns cost the plain distance. Since the penalty is at most 1, it only
    # changes the outcome at the boundary distance == FUZZERR.
    d <- as.integer(adist(qp, rp))
    keep <- !is.na(d) & d < fuzzerr
    bnd <- which(!is.na(d) & d == fuzzerr)
    if (length(bnd)) {
      keep[bnd] <- .tt_amatch_trailing(qp, rp[bnd], d[bnd]) == 0L
    }
    codes <- cand[keep]
    if (!length(codes)) {
      return(NULL)
    }
    hit(codes, "auto_fuzzy")
  }

  # Accumulate matches as parallel (query index, ref code, type) vectors
  n_q <- length(A$code)
  res_qi <- vector("list", n_q)
  res_rc <- vector("list", n_q)
  res_type <- vector("list", n_q)

  for (qi in seq_len(n_q)) {
    res <- test_eq(qi, "exact", "exact")
    if (is.null(res)) res <- test_eq(qi, "punct", "auto_punct")
    if (is.null(res) && A$no_auth[qi] && noauthone) res <- test_na(qi)
    if (is.null(res)) res <- test_pm(qi, "basio")
    if (is.null(res)) res <- test_pm(qi, "in")
    if (is.null(res)) res <- test_pm(qi, "exin")
    if (is.null(res)) res <- test_eq(qi, "basexin", "auto_basexin")
    if (is.null(res)) res <- test_eq(qi, "irank", "auto_irank")

    if (is.null(res) && A$auct[qi]) {
      # auct.: only a canonical-form match is allowed, else no_match
      if (cfo) {
        res <- test_eq(qi, "cfonly", "auto_cfonly")
      }
    } else {
      # all-fuzzy (only if genus present in reference)
      if (is.null(res) && A$genus[qi] %in% Bgenus_present) {
        res <- test_allfuzzy(qi)
      }
      if (is.null(res) && cfo) res <- test_eq(qi, "cfonly", "auto_cfonly")
    }

    if (is.null(res)) res <- hit(NA_character_, "no_match")
    res_qi[[qi]] <- rep(qi, length(res$codes))
    res_rc[[qi]] <- res$codes
    res_type[[qi]] <- rep(res$type, length(res$codes))
  }

  qi <- unlist(res_qi, use.names = FALSE)
  rc <- unlist(res_rc, use.names = FALSE)
  type <- unlist(res_type, use.names = FALSE)

  # Ref row for each matched code. If a code is duplicated in the reference,
  # the last record wins (as in the lookup maps).
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
