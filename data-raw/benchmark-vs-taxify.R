# Benchmark: taxastand vs taxify on author-sensitive names, typos, and speed.
#
# Not part of the package and not run in CI: taxify matches against backbones
# that must be downloaded first (install_backbones("wfo") is ~800 MB), so run
# this by hand:
#
#   Rscript data-raw/benchmark-vs-taxify.R [ppg.csv] [taxify_backbone] [out_dir] [reference]
#
# Defaults: PPG v2.0.0 is downloaded to a temp file; taxify backbone "wfo";
# results go to data-raw/benchmark-results/; reference "ppg". Run from the
# package root (uses devtools::load_all(), so it measures the working tree). If
# taxify is not installed, only the taxastand half runs (useful as a smoke
# test).
#
# `reference` is what taxastand matches against:
# - "ppg" (default): PPG, ~72k ferns and lycophytes. taxify still matches
#   against its backbone (WFO, ~1.6M names), so the two tools see different
#   references and the speed comparison favours the smaller one.
# - "taxify": taxify's installed backbone, read from its data directory (needs
#   taxify and its vectra dependency). Both tools then match the same names, so
#   speed and accuracy are like for like. Expect this run to take a while
#   (several minutes with "wfo"): matching against 1.6M names without a
#   prepared reference is slow.
#
# What is compared
# - Homonyms (author-sensitive): PPG names whose canonical name (genus +
#   epithet [+ infraspecific epithet]) is shared by records with different
#   authors that resolve to *different* accepted names, e.g. Trichomanes
#   bifidum Vent. ex Willd. vs C.Presl. Each pair is queried with its authors.
#   Metric: how often the two authored queries come back with different
#   accepted names (taxonomy independent: the truth is that they differ).
# - Typos: PPG names with one letter substituted, two adjacent letters
#   swapped, or one letter deleted in the epithet, with and without author.
#   Metric: is the original canonical name among the hits? (Evaluated only on
#   names that taxify's backbone also holds, found by querying them unmodified.)
# - Speed: elapsed time for 1, 10, 100, 1000 typo queries (best of 3, after a
#   warm-up call so taxify's one-off backbone load is not counted).
#
# The two tools do different jobs (taxastand matches against a reference you
# supply, here PPG; taxify against its downloaded backbones), so read the
# numbers as "how each handles authors, typos and size", not as a ranking.

args <- commandArgs(trailingOnly = TRUE)
ppg_version <- "v2.0.0"
ppg_path <- if (length(args) >= 1) args[[1]] else file.path(tempdir(), "ppg.csv")
bb <- if (length(args) >= 2) args[[2]] else "wfo"
out_dir <- if (length(args) >= 3) args[[3]] else "data-raw/benchmark-results"
reference <- if (length(args) >= 4) args[[4]] else "ppg"
stopifnot(reference %in% c("ppg", "taxify"))
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
set.seed(20261107)

if (reference == "ppg" && !file.exists(ppg_path)) {
  utils::download.file(
    sprintf("https://raw.githubusercontent.com/pteridogroup/ppg/%s/data/ppg.csv", ppg_version),
    ppg_path, quiet = TRUE
  )
}
suppressMessages(devtools::load_all(quiet = TRUE))
have_taxify <- requireNamespace("taxify", quietly = TRUE)
if (!have_taxify) message("taxify is not installed: running the taxastand half only")
if (reference == "taxify" && !have_taxify) {
  stop("reference = \"taxify\" needs taxify installed (its backbone is the reference)")
}

# ---- Reference -----------------------------------------------------------------
# One row per name, with the columns used below (named after PPG's):
# scientificName (canonical name), scientificNameAuthorship, taxonRank,
# taxonID, acceptedNameUsageID (NA for accepted names), taxonomicStatus
load_ppg <- function() {
  x <- utils::read.csv(ppg_path, stringsAsFactors = FALSE, na.strings = "NA")
  x[!is.na(x$scientificName), ]
}
# taxify keeps each backbone as a vectra file in its data directory
load_taxify_backbone <- function(bb) {
  f <- file.path(taxify::taxify_data_dir(), bb, "latest", paste0(bb, ".vtr"))
  if (!file.exists(f)) stop("taxify backbone not found: ", f)
  x <- as.data.frame(vectra::collect(vectra::tbl(f)))
  need <- c(
    "taxon_id", "canonical_name", "authorship", "taxon_rank",
    "taxonomic_status", "accepted_taxon_id"
  )
  if (!all(need %in% colnames(x))) {
    stop("taxify backbone lacks columns: ", paste(setdiff(need, colnames(x)), collapse = ", "))
  }
  data.frame(
    taxonID = x$taxon_id, scientificName = x$canonical_name,
    scientificNameAuthorship = x$authorship, taxonRank = tolower(x$taxon_rank),
    taxonomicStatus = tolower(x$taxonomic_status),
    acceptedNameUsageID = ifelse(
      x$accepted_taxon_id == x$taxon_id, NA_character_, x$accepted_taxon_id
    ),
    stringsAsFactors = FALSE
  )
}
ppg <- if (reference == "ppg") load_ppg() else load_taxify_backbone(bb)
ppg <- ppg[ppg$taxonRank %in% c("species", "subspecies", "variety", "form"), ]
ppg$canonical <- ppg$scientificName
has_auth <- !is.na(ppg$scientificNameAuthorship) & ppg$scientificNameAuthorship != ""
ppg$scientificNameAuthorship[!has_auth] <- NA
ppg$full <- ppg$canonical
ppg$full[has_auth] <- paste(ppg$canonical[has_auth], ppg$scientificNameAuthorship[has_auth])
ppg <- ppg[!duplicated(ppg$full), ]
ref_tax <- data.frame(
  taxonID = ppg$taxonID, acceptedNameUsageID = ppg$acceptedNameUsageID,
  taxonomicStatus = ppg$taxonomicStatus, scientificName = ppg$full,
  stringsAsFactors = FALSE
)
cat("Reference:", reference, " names:", nrow(ref_tax), "\n")
accepted_canonical <- function(i) {
  id <- ppg$acceptedNameUsageID[i]
  j <- match(id, ppg$taxonID)
  ifelse(is.na(id) | is.na(j), ppg$canonical[i], ppg$canonical[j])
}
ppg$accepted <- accepted_canonical(seq_len(nrow(ppg)))

# ---- Tool wrappers -----------------------------------------------------------
# Each returns a data frame with one row per hit: query, hit (canonical name),
# accepted (accepted canonical name, NA if not resolved)
# `reference` is the names, or the output of ts_prepare_reference()
make_taxastand <- function(reference, closest = FALSE) {
  hits <- if (closest) "closest" else "all"
  function(q) {
    m <- suppressWarnings(ts_match_names(
      q, reference, simple = TRUE, fuzzy_canon_hits = hits
    ))
    m <- m[m$match_type != "no_match", ]
    data.frame(
      query = m$query, hit = ppg$canonical[match(m$reference, ppg$full)],
      accepted = ppg$accepted[match(m$reference, ppg$full)],
      full = m$reference, stringsAsFactors = FALSE
    )
  }
}
run_taxastand <- make_taxastand(ref_tax$scientificName)
run_taxastand_closest <- make_taxastand(ref_tax$scientificName, closest = TRUE)
# A prepared reference gives the same results; accuracy is evaluated with it
# (faster), and speed is timed both ways
ref_prepared <- ts_prepare_reference(ref_tax)
run_taxastand_prepared <- make_taxastand(ref_prepared)
run_taxastand_prepared_closest <- make_taxastand(ref_prepared, closest = TRUE)
resolve_taxastand <- function(q) {
  r <- suppressWarnings(ts_resolve_names(q, ref_prepared))
  r$resolved_name[match(q, r$query)]
}
run_taxify <- function(q) {
  r <- suppressWarnings(suppressMessages(
    taxify::taxify(q, backbone = bb, verbose = FALSE)
  ))
  r <- as.data.frame(r)
  ok <- !is.na(r$matched_name)
  data.frame(
    query = r$input_name[ok], hit = r$matched_name[ok],
    accepted = r$accepted_name[ok],
    full = trimws(paste(r$matched_name[ok], r$authorship[ok])),
    stringsAsFactors = FALSE
  )
}

# ---- Evaluable set: names the taxify backbone holds --------------------------
canon_pool <- unique(ppg$canonical[ppg$taxonRank == "species"])
if (have_taxify && reference == "ppg") {
  probe <- run_taxify(canon_pool)
  held <- canon_pool[canon_pool %in% probe$hit[probe$query == probe$hit]]
} else {
  held <- canon_pool
}
cat("Names in reference:", length(canon_pool), " also in backbone:", length(held), "\n")

# ---- 1. Homonyms -------------------------------------------------------------
hom <- ppg[!is.na(ppg$scientificNameAuthorship) & ppg$canonical %in% held, ]
# only canonical names with more than one author can form a pair; sample them
# first so a large reference does not make this loop slow
multi <- unique(hom$canonical[duplicated(hom$canonical)])
if (length(multi) > 2000) multi <- sample(multi, 2000)
hom <- hom[hom$canonical %in% multi, ]
by_name <- split(seq_len(nrow(hom)), hom$canonical)
pairs <- do.call(rbind, lapply(by_name, function(ix) {
  if (length(ix) < 2) return(NULL)
  ix <- ix[!duplicated(hom$accepted[ix])]
  if (length(ix) < 2) return(NULL)
  data.frame(a = hom$full[ix[1]], b = hom$full[ix[2]], stringsAsFactors = FALSE)
}))
cat("Homonym pairs (different accepted names):", nrow(pairs), "\n")
if (nrow(pairs) > 500) pairs <- pairs[sample(nrow(pairs), 500), ]
discriminates <- function(run) {
  qs <- unique(c(pairs$a, pairs$b))
  h <- run(qs)
  acc <- function(q) {
    x <- unique(h$accepted[h$query == q])
    if (length(x) == 1) x else NA_character_
  }
  aa <- vapply(pairs$a, acc, "")
  bb_ <- vapply(pairs$b, acc, "")
  # "held": the tool returned the very name queried, author included (taxify
  # can only separate homonyms its backbone holds; it reports a match with a
  # different author's name as `exact`)
  held_q <- function(q) {
    f <- h$full[h$query == q]
    any(tt_depunct(f) == tt_depunct(q))
  }
  held <- vapply(pairs$a, held_q, NA) & vapply(pairs$b, held_q, NA)
  ok <- !is.na(aa) & !is.na(bb_)
  out <- c(
    pairs = nrow(pairs),
    both_resolved = sum(ok),
    different = sum(ok & aa != bb_),
    both_held = sum(held),
    different_among_held = sum(held & ok & aa != bb_)
  )
  # per-pair accepted names, to inspect the pairs a tool failed to separate
  attr(out, "detail") <- data.frame(
    a = pairs$a, b = pairs$b, accepted_a = aa, accepted_b = bb_,
    stringsAsFactors = FALSE
  )
  out
}
homonyms <- list(taxastand = discriminates(run_taxastand_prepared))
# taxastand's own check against PPG truth (resolved to the right accepted name)
truth_a <- ppg$accepted[match(pairs$a, ppg$full)]
truth_b <- ppg$accepted[match(pairs$b, ppg$full)]
to_canonical <- function(x) ppg$canonical[match(x, ppg$full)]
ra <- to_canonical(resolve_taxastand(pairs$a))
rb <- to_canonical(resolve_taxastand(pairs$b))
homonyms$taxastand_correct_vs_ppg <- sum(
  ra == truth_a & rb == truth_b, na.rm = TRUE
)
if (have_taxify) homonyms$taxify <- discriminates(run_taxify)

# ---- 2. Typos ----------------------------------------------------------------
sp <- ppg[ppg$taxonRank == "species" & ppg$canonical %in% held & !is.na(ppg$scientificNameAuthorship), ]
sp <- sp[sample(nrow(sp), min(300, nrow(sp))), ]
genus <- sub(" .*", "", sp$canonical)
epi <- sub("^\\S+ (\\S+).*", "\\1", sp$canonical)
sub1 <- function(e) {
  i <- sample(nchar(e), 1)
  `substr<-`(e, i, i, "q")
}
swp <- function(e) {
  if (nchar(e) < 3) return(e)
  i <- sample(nchar(e) - 1, 1)
  a <- substr(e, i, i)
  b <- substr(e, i + 1, i + 1)
  `substr<-`(e, i, i + 1, paste0(b, a))
}
del <- function(e) {
  i <- sample(nchar(e), 1)
  paste0(substr(e, 1, i - 1), substr(e, i + 1, nchar(e)))
}
kinds <- list(substitution = sub1, swap = swp, deletion = del)
typos <- do.call(rbind, lapply(names(kinds), function(k) {
  e2 <- vapply(epi, kinds[[k]], "", USE.NAMES = FALSE)
  keep <- e2 != epi
  base <- paste(genus, e2)
  rbind(
    data.frame(kind = k, authored = FALSE, query = base[keep], truth = sp$canonical[keep]),
    data.frame(kind = k, authored = TRUE,
      query = paste(base, sub("^\\S+ \\S+ ?", "", sp$full))[keep], truth = sp$canonical[keep])
  )
}))
typos <- typos[!duplicated(typos$query), ]
recall <- function(run) {
  h <- run(typos$query)
  typos$found <- mapply(function(q, t) t %in% h$hit[h$query == q], typos$query, typos$truth)
  typos$n_hits <- vapply(typos$query, function(q) sum(h$query == q), 0L)
  agg <- stats::aggregate(cbind(found, n_hits) ~ kind + authored, typos, mean)
  agg$n <- as.vector(table(typos$kind, typos$authored)[cbind(agg$kind, ifelse(agg$authored, "TRUE", "FALSE"))])
  agg
}
# taxastand returns every hit within max_dist by default; "closest" keeps only
# the nearest ones, which is the fairer comparison with taxify's single pick
typo_res <- list(
  taxastand = recall(run_taxastand_prepared),
  taxastand_closest = recall(run_taxastand_prepared_closest)
)
if (have_taxify) typo_res$taxify <- recall(run_taxify)

# ---- 3. Speed ----------------------------------------------------------------
timing <- function(run, label) {
  run(typos$query[1:5]) # warm-up (loads taxify's backbone)
  do.call(rbind, lapply(c(1, 10, 100, 1000), function(n) {
    q <- rep_len(typos$query, n)
    q <- unique(q)
    if (length(q) < n) q <- c(q, paste0(q[seq_len(n - length(q))], "x"))
    t <- min(vapply(1:3, function(i) unname(system.time(run(q))[3]), 0))
    data.frame(tool = label, n_queries = n, seconds = round(t, 2))
  }))
}
# "taxastand" prepares the reference on every call; "taxastand_prepared" reuses
# a prepared reference (best of 3 runs, so each genus is parsed before the
# timed repeat), the like-for-like counterpart of taxify's pre-loaded backbone
speed <- timing(run_taxastand, "taxastand")
speed <- rbind(speed, timing(run_taxastand_closest, "taxastand_closest"))
speed <- rbind(speed, timing(run_taxastand_prepared, "taxastand_prepared"))
speed <- rbind(speed, timing(run_taxastand_prepared_closest, "taxastand_prepared_closest"))
if (have_taxify) speed <- rbind(speed, timing(run_taxify, "taxify"))

# ---- Report and save ---------------------------------------------------------
cat("\n== Homonyms (pairs whose two authored queries should differ) ==\n")
print(lapply(homonyms, function(x) {
  attr(x, "detail") <- NULL
  x
}))
cat("\n== Typos: share of queries whose original name is among the hits ==\n")
print(typo_res)
cat("\n== Speed (seconds, best of 3) ==\n")
print(speed)

res <- list(
  date = Sys.time(), reference = reference, reference_names = nrow(ref_tax),
  ppg_version = if (reference == "ppg") ppg_version, taxify_backbone = if (have_taxify) bb,
  taxastand_version = as.character(utils::packageVersion("taxastand")),
  taxify_version = if (have_taxify) as.character(utils::packageVersion("taxify")),
  r_version = R.version.string, platform = R.version$platform,
  homonym_pairs = pairs, homonyms = homonyms,
  homonym_detail = lapply(homonyms[c("taxastand", "taxify")], attr, "detail"), typo_queries = typos,
  typos = typo_res, speed = speed
)
saveRDS(res, file.path(out_dir, "benchmark-vs-taxify.rds"))
utils::write.csv(speed, file.path(out_dir, "speed.csv"), row.names = FALSE)
writeLines(utils::capture.output(utils::sessionInfo()), file.path(out_dir, "session-info.txt"))
cat("\nResults written to", out_dir, "\n")
