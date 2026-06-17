# Pure-R reimplementation of taxon-tools `matchnames` (the `-F` / ALLFUZZY,
# non-interactive mode, which is the only mode taxastand uses).
#
# Faithful port of the gawk script `matchnames` from taxon-tools v1.3.0
# (camwebb/taxon-tools, upstream commit 8f8b5e2). The interactive `-f`
# matching, the `-m` manual-choices file, and stdin prompting are intentionally
# omitted (out of scope; never invoked by taxastand).
#
# Input is the parsed pipe-delimited format produced by tt_parsenames /
# ts_write_names: id|g_hybrid|genus|s_hybrid|species|rank|infrasp|author
# Output is the 17-field result format documented in ts_match_names().
#
# Fuzzy matching uses base adist() (Levenshtein); the original uses gawk's
# anchored amatch(), documented in-source as equivalent to
# levenshtein(...) <= FUZZERR.

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

  data <- paste(xg, g, xs, s, st, ssp, a, sep = "|")
  exact <- .tt_cleanspaces(paste(xg, g, xs, s, st, ssp, a))

  namex <- paste0(xg, g, xs, s, st, ssp)         # all parts, no author
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
    code = code, data = data, genus = g,
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

#' Match taxonomic names (taxon-tools `matchnames -F`)
#'
#' Faithful pure-R port of the non-interactive all-fuzzy matching mode.
#'
#' @param query_lines,ref_lines Character vectors of parsed `id|...` lines.
#' @param max_dist Integer; max Levenshtein distance for fuzzy matching.
#' @param match_no_auth Logical; the `-1` option (NOAUTHONE).
#' @param match_canon Logical; the `-c` option (CFO / canonical-form match).
#' @return Character vector of 17-field `|`-delimited result lines.
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

  methods <- c("exact", "punct", "noauth", "cfonly", "irank",
               "basio", "in", "exin", "basexin")
  Bmap <- lapply(methods, function(m) .tt_build_map(B[[m]], B$code))
  names(Bmap) <- methods

  # noauth-key counts (XNA) and code -> data lookup
  Bxna <- table(B$noauth)
  Bdata <- .tt_build_map(B$code, B$data)

  # Genus -> ascending-sorted ref codes (Gh, iterated in @ind_str_asc order)
  Bgenus_codes <- lapply(split(B$code, B$genus), function(x) sort(unique(x)))
  Bgenus_present <- names(Bgenus_codes)
  # punct keyed by code for fuzzy candidate lookup
  Bpunct_by_code <- setNames(B$punct, B$code)

  out <- vector("list", length(A$code))

  # emit a standard 17-field line: qcode|rcode|type|<qdata>|<rdata>
  line_match <- function(qi, rcode, type) {
    paste0(A$code[qi], "|", rcode, "|", type, "|",
           A$data[qi], "|", .tt_get(Bdata, rcode))
  }
  line_nomatch <- function(qi) {
    paste0(A$code[qi], "||no_match|", A$data[qi], "|||||||")
  }

  # test_eq: query[method] key present in B[method]
  test_eq <- function(qi, method, outtype) {
    rc <- .tt_get(Bmap[[method]], A[[method]][qi])
    if (!is.na(rc) && rc != A$code[qi]) line_match(qi, rc, outtype) else NULL
  }
  # test_na: noauth key unique in B
  test_na <- function(qi) {
    key <- A$noauth[qi]
    rc <- .tt_get(Bmap[["noauth"]], key)
    cnt <- Bxna[key]
    if (!is.na(rc) && !is.na(cnt) && cnt == 1 && rc != A$code[qi]) {
      line_match(qi, rc, "auto_noauth")
    } else {
      NULL
    }
  }
  # test_pm: plus/minus variants using punct on the opposite side
  test_pm <- function(qi, method) {
    rc <- .tt_get(Bmap[[method]], A$punct[qi])
    if (!is.na(rc) && rc != A$code[qi]) {
      return(line_match(qi, rc, paste0("auto_", method, "-")))
    }
    rc <- .tt_get(Bmap[["punct"]], A[[method]][qi])
    if (!is.na(rc) && rc != A$code[qi]) {
      return(line_match(qi, rc, paste0("auto_", method, "+")))
    }
    NULL
  }
  # test_allfuzzy: every same-genus ref within max_dist
  test_allfuzzy <- function(qi) {
    cand <- Bgenus_codes[[A$genus[qi]]]
    cand <- cand[cand != A$code[qi]]
    if (!length(cand)) return(NULL)
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
    hit <- cand[keep]
    if (!length(hit)) return(NULL)
    vapply(hit, function(rc) line_match(qi, rc, "auto_fuzzy"), character(1))
  }

  for (qi in seq_along(A$code)) {
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
      out[[qi]] <- if (is.null(res)) line_nomatch(qi) else res
      next
    }

    # all-fuzzy (only if genus present in reference)
    if (is.null(res) && A$genus[qi] %in% Bgenus_present) {
      res <- test_allfuzzy(qi)
    }
    if (is.null(res) && cfo) res <- test_eq(qi, "cfonly", "auto_cfonly")

    out[[qi]] <- if (is.null(res)) line_nomatch(qi) else res
  }

  unlist(out, use.names = FALSE)
}
