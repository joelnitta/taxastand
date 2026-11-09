# Helpers for data-raw/benchmark-vignette.R: reference loading, synthetic query
# generation, scoring and one adapter per package.
#
# Design
# - Every comparison is paired: taxastand and the other package match against
#   exactly the same reference table, so differences come from the matching, not
#   from different taxonomies.
# - Queries are built by damaging names taken from that reference, so the right
#   answer is known by construction (the record the query was made from).
# - Matches are scored by record ID. An adapter takes a character vector of
#   queries and returns a data frame (query, id), one row per record returned.

# ---- reference ----
# One row per distinct authored name, with columns:
#   taxonID, canonical, authorship (NA if none), full (canonical + authorship),
#   taxonRank (lower case), taxonomicStatus, acceptedNameUsageID (NA if accepted)
load_reference <- function(source = c("ppg", "wfo"), ppg_path = NULL,
                           ppg_version = "v2.0.0", wfo_backbone = "wfo") {
  source <- match.arg(source)
  if (source == "ppg") {
    if (is.null(ppg_path)) ppg_path <- file.path(tempdir(), "ppg.csv")
    if (!file.exists(ppg_path)) {
      utils::download.file(
        sprintf(
          "https://raw.githubusercontent.com/pteridogroup/ppg/%s/data/ppg.csv",
          ppg_version
        ),
        ppg_path,
        quiet = TRUE
      )
    }
    x <- utils::read.csv(ppg_path, stringsAsFactors = FALSE, na.strings = "NA")
    x <- x[!is.na(x$scientificName), ]
  } else {
    # taxify keeps each backbone as a vectra file in its data directory
    f <- file.path(
      taxify::taxify_data_dir(), wfo_backbone, "latest",
      paste0(wfo_backbone, ".vtr")
    )
    if (!file.exists(f)) stop("taxify backbone not found: ", f)
    b <- as.data.frame(vectra::collect(vectra::tbl(f)))
    x <- data.frame(
      taxonID = b$taxon_id, scientificName = b$canonical_name,
      scientificNameAuthorship = b$authorship,
      taxonRank = tolower(b$taxon_rank),
      taxonomicStatus = tolower(b$taxonomic_status),
      acceptedNameUsageID = ifelse(
        b$accepted_taxon_id == b$taxon_id, NA_character_, b$accepted_taxon_id
      ),
      stringsAsFactors = FALSE
    )
  }
  x <- x[x$taxonRank %in% c("species", "subspecies", "variety", "form"), ]
  auth <- x$scientificNameAuthorship
  has_auth <- !is.na(auth) & auth != ""
  full <- x$scientificName
  full[has_auth] <- paste(x$scientificName[has_auth], auth[has_auth])
  out <- data.frame(
    taxonID = x$taxonID, canonical = x$scientificName,
    authorship = ifelse(has_auth, auth, NA_character_), full = full,
    taxonRank = x$taxonRank, taxonomicStatus = x$taxonomicStatus,
    acceptedNameUsageID = x$acceptedNameUsageID,
    stringsAsFactors = FALSE
  )
  out[!duplicated(out$full), ]
}

# ---- damage ----
# Damage one letter of `x`: substitution, deletion, or swap of two neighbours.
# `from` is the first position that may be changed.
damage <- function(x, kind, from = 1L) {
  n <- nchar(x)
  if (n < from + 1L) {
    return(x)
  }
  i <- if (n - from > 0L) from - 1L + sample.int(n - from, 1L) else from
  switch(kind,
    substitution = `substr<-`(x, i, i, if (substr(x, i, i) == "q") "z" else "q"),
    deletion = paste0(substr(x, 1, i - 1), substr(x, i + 1, n)),
    swap = {
      i <- min(i, n - 1L)
      a <- substr(x, i, i)
      b <- substr(x, i + 1L, i + 1L)
      if (a == b) x else `substr<-`(x, i, i + 1L, paste0(b, a))
    }
  )
}

# ---- make-queries ----
# Queries made from random species-rank records that have an author, `n` per
# kind of error. Returns a data frame: query, kind, source (row of `ref`) and
# `truth` (a list of the record IDs that count as the right answer: the record
# itself, or, when the query has no author, every record sharing the canonical
# name).
#   exact, no_author_exact     the name as in the reference (without author)
#   epithet_*, genus_deletion  one letter changed, with the author
#   two_edits                  two letters changed in the epithet
#   short_epithet              one deletion in an epithet of five letters or less
#   author_punctuation         periods removed from the author
#   author_typo                a letter deleted in the author
#   homonym, homonym_typo      a name shared by records with different authors,
#                              queried with its own author (as is, or with a
#                              typo in the epithet): needs the author to decide
make_queries <- function(ref, n = 100, seed = 20261109) {
  set.seed(seed)
  species <- ref$taxonRank == "species" & !is.na(ref$authorship) &
    grepl("^[A-Z][a-z]+ [a-z-]+$", ref$canonical)
  epithet <- sub("^\\S+ ", "", ref$canonical)
  dup_canon <- ref$canonical %in% ref$canonical[duplicated(ref$canonical)]
  pools <- list(
    default = which(species & nchar(ref$canonical) > 12),
    short = which(species & nchar(epithet) <= 5 & nchar(epithet) >= 3),
    homonym = which(species & dup_canon)
  )
  by_canon <- split(ref$taxonID, ref$canonical)
  # name parts of record i
  parts <- function(src) {
    list(
      genus = sub(" .*", "", ref$canonical[src]), epi = epithet[src],
      auth = ref$authorship[src]
    )
  }
  dmg <- function(x, kind, from = 1L) vapply(x, damage, "", kind, from = from)
  author_typo <- function(a) {
    vapply(a, function(x) {
      w <- strsplit(x, "(?<=[ .])", perl = TRUE)[[1]]
      k <- which.max(nchar(w))
      w[k] <- damage(w[k], "deletion", from = 2L)
      paste(w, collapse = "")
    }, "")
  }
  kinds <- list(
    exact = list("default", function(p) paste(p$genus, p$epi, p$auth)),
    epithet_substitution = list("default", function(p) {
      paste(p$genus, dmg(p$epi, "substitution"), p$auth)
    }),
    epithet_swap = list("default", function(p) {
      paste(p$genus, dmg(p$epi, "swap"), p$auth)
    }),
    epithet_deletion = list("default", function(p) {
      paste(p$genus, dmg(p$epi, "deletion"), p$auth)
    }),
    two_edits = list("default", function(p) {
      paste(p$genus, dmg(dmg(p$epi, "deletion"), "substitution"), p$auth)
    }),
    short_epithet = list("short", function(p) {
      paste(p$genus, dmg(p$epi, "deletion"), p$auth)
    }),
    genus_deletion = list("default", function(p) {
      paste(dmg(p$genus, "deletion", from = 2L), p$epi, p$auth)
    }),
    no_author_exact = list("default", function(p) paste(p$genus, p$epi)),
    no_author_epithet_deletion = list("default", function(p) {
      paste(p$genus, dmg(p$epi, "deletion"))
    }),
    author_punctuation = list("default", function(p) {
      paste(p$genus, p$epi, gsub(".", "", p$auth, fixed = TRUE))
    }),
    author_typo = list("default", function(p) {
      paste(p$genus, p$epi, author_typo(p$auth))
    }),
    homonym = list("homonym", function(p) paste(p$genus, p$epi, p$auth)),
    homonym_typo = list("homonym", function(p) {
      paste(p$genus, dmg(p$epi, "deletion"), p$auth)
    })
  )
  out <- do.call(rbind, lapply(names(kinds), function(k) {
    pool <- pools[[kinds[[k]][[1]]]]
    src <- pool[sample.int(length(pool), min(n, length(pool)))]
    q <- trimws(kinds[[k]][[2]](parts(src)))
    data.frame(query = q, kind = k, source = src, stringsAsFactors = FALSE)
  }))
  out$truth <- lapply(seq_len(nrow(out)), function(i) {
    if (grepl("^no_author", out$kind[i])) {
      by_canon[[ref$canonical[out$source[i]]]]
    } else {
      ref$taxonID[out$source[i]]
    }
  })
  out$original <- ref$full[out$source]
  # a query string can arise from different records or kinds; keep each once
  out[!duplicated(out$query), ]
}

# ---- scoring ----
# `hits` has one row per record returned: query, id, and `pick` (TRUE for the
# record the tool itself chooses as its answer; at most one per query).
# One row per query: n_hits (records returned), correct (the right record is
# among them), unambiguous (it is the only one returned), picked (the tool chose
# a record) and pick_correct (the chosen record is right).
score_queries <- function(queries, hits) {
  hits <- hits[!duplicated(hits[, c("query", "id")]), ]
  lv <- queries$query
  by_q <- split(hits$id, factor(hits$query, levels = lv))
  picks <- split(
    hits$id[hits$pick], factor(hits$query[hits$pick], levels = lv)
  )
  n_hits <- vapply(by_q, length, 0L)
  correct <- mapply(function(h, t) any(h %in% t), by_q, queries$truth)
  picked <- vapply(picks, length, 0L) > 0
  pick_correct <- mapply(
    function(p, t) length(p) > 0 && p[[1]] %in% t, picks, queries$truth
  )
  data.frame(
    query = queries$query, kind = queries$kind, n_hits = unname(n_hits),
    correct = unname(correct), unambiguous = unname(correct & n_hits == 1L),
    picked = unname(picked), pick_correct = unname(pick_correct),
    stringsAsFactors = FALSE
  )
}

# Per kind of error:
# - recall: share of all queries for which the right record is among those
#   returned; precision: the same share among queries that got an answer
# - unambiguous: share where the right record is the only one returned;
#   mean_hits: average records returned per answered query
# - pick_answered: share of queries for which the tool chose one record;
#   pick_precision: share of those choices that are right
summarise_scores <- function(scores) {
  f <- function(s) {
    answered <- s$n_hits > 0
    data.frame(
      n = nrow(s), answered = mean(answered), recall = mean(s$correct),
      precision = if (any(answered)) mean(s$correct[answered]) else NA_real_,
      unambiguous = mean(s$unambiguous),
      mean_hits = if (any(answered)) mean(s$n_hits[answered]) else NA_real_,
      pick_answered = mean(s$picked),
      pick_precision = if (any(s$picked)) mean(s$pick_correct[s$picked]) else NA_real_
    )
  }
  out <- do.call(rbind, lapply(split(scores, scores$kind), f))
  out$kind <- rownames(out)
  rownames(out) <- NULL
  out[, c("kind", setdiff(colnames(out), "kind"))]
}

# ---- adapter-taxastand ----
# Each make_*() takes the reference table and returns a list with `setup_s` (the
# time spent preparing the reference, once) and `run(queries)`.

make_taxastand <- function(ref, prepared = TRUE) {
  tax <- data.frame(
    taxonID = ref$taxonID, acceptedNameUsageID = ref$acceptedNameUsageID,
    taxonomicStatus = ref$taxonomicStatus, scientificName = ref$full,
    stringsAsFactors = FALSE
  )
  setup <- system.time(
    reference <- if (prepared) taxastand::ts_prepare_reference(tax) else tax$scientificName
  )[["elapsed"]]
  list(
    setup_s = setup,
    run = function(q) {
      m <- suppressWarnings(
        taxastand::ts_match_names(q, reference, simple = TRUE)
      )
      m <- m[m$match_type != "no_match", ]
      h <- data.frame(
        query = m$query, id = ref$taxonID[match(m$reference, ref$full)],
        stringsAsFactors = FALSE
      )
      # taxastand does not guess: its answer is the record when only one fits
      n_ids <- tapply(h$id, h$query, function(x) length(unique(x)))
      h$pick <- n_ids[h$query] == 1L
      h
    }
  )
}

# ---- adapter-taxify ----
make_taxify <- function(ref, backbone = "wfo") {
  setup <- system.time(
    taxify::taxify("Quercus robur", backbone = backbone, verbose = FALSE)
  )[["elapsed"]]
  list(
    setup_s = setup,
    run = function(q) {
      r <- as.data.frame(suppressWarnings(suppressMessages(
        taxify::taxify(q, backbone = backbone, verbose = FALSE)
      )))
      ok <- !is.na(r$taxon_id)
      # taxify returns one record per query: that is its pick
      data.frame(
        query = r$input_name[ok], id = r$taxon_id[ok], pick = TRUE,
        stringsAsFactors = FALSE
      )
    }
  )
}

# ---- adapter-utaxonstand ----
make_utaxonstand <- function(ref) {
  genus <- sub(" .*", "", ref$canonical)
  setup <- system.time({
    db <- data.frame(
      ID = ref$taxonID, Name = ref$canonical,
      Author = ifelse(is.na(ref$authorship), "", ref$authorship),
      Genus = genus,
      Rank = ifelse(ref$taxonRank == "species", 2, 3),
      ACCEPTED_ID = ref$acceptedNameUsageID,
      stringsAsFactors = FALSE
    )
    db <- U.Taxonstand::nameClean(db)
  })[["elapsed"]]
  list(
    setup_s = setup,
    run = function(q) {
      # split the query into name (genus + epithet) and author
      m <- regmatches(q, regexec("^(\\S+ \\S+)\\s*(.*)$", q))
      sp <- data.frame(
        Name = vapply(m, `[`, "", 2), Author = vapply(m, `[`, "", 3),
        Rank = 2, stringsAsFactors = FALSE
      )
      # nameMatch() prints progress bars; discard them
      utils::capture.output(r <- suppressWarnings(suppressMessages(
        U.Taxonstand::nameMatch(
          spList = sp, spSource = db, author = TRUE, matchFirst = FALSE
        )
      )))
      # one row per matched record; Name_in_database may be empty on no match
      ok <- !is.na(r$ID_in_database) & r$ID_in_database != ""
      idx <- match(
        paste(r$Submitted_Name, r$Submitted_Author),
        paste(sp$Name, sp$Author)
      )
      # Name_set == 1 marks the best match of each query
      h <- data.frame(
        query = q[idx[ok]], id = r$ID_in_database[ok],
        pick = r$Name_set[ok] == 1, stringsAsFactors = FALSE
      )
      h$pick <- h$pick & !duplicated(cbind(h$query, h$pick))
      h
    }
  )
}

# ---- adapter-worldflora ----
make_worldflora <- function(ref) {
  parts <- strsplit(ref$canonical, " ", fixed = TRUE)
  setup <- system.time({
    wfo <- data.frame(
      taxonID = ref$taxonID, scientificNameID = "",
      scientificName = ref$canonical,
      taxonRank = toupper(ref$taxonRank), parentNameUsageID = "",
      scientificNameAuthorship = ifelse(is.na(ref$authorship), "", ref$authorship),
      family = "", genus = vapply(parts, `[`, "", 1),
      specificEpithet = vapply(parts, `[`, "", 2),
      infraspecificEpithet = vapply(parts, function(p) {
        if (length(p) > 2) p[length(p)] else ""
      }, ""),
      verbatimTaxonRank = "", nomenclaturalStatus = "", namePublishedIn = "",
      taxonomicStatus = tools::toTitleCase(ref$taxonomicStatus),
      acceptedNameUsageID = ifelse(
        is.na(ref$acceptedNameUsageID), "", ref$acceptedNameUsageID
      ),
      nameAccordingToID = "", stringsAsFactors = FALSE
    )
  })[["elapsed"]]
  list(
    setup_s = setup,
    run = function(q) {
      m <- regmatches(q, regexec("^(\\S+ \\S+)\\s*(.*)$", q))
      spec <- data.frame(
        spec.name = vapply(m, `[`, "", 2), Authorship = vapply(m, `[`, "", 3),
        key = q, stringsAsFactors = FALSE
      )
      # acceptedNameUsageID.match = FALSE: return the record that matched, not
      # the accepted record it points to, so that matching is scored alone
      utils::capture.output(r <- suppressWarnings(suppressMessages(
        WorldFlora::WFO.match(
          spec.data = spec, WFO.data = wfo, Authorship = "Authorship",
          acceptedNameUsageID.match = FALSE, no.dates = TRUE, verbose = FALSE,
          counter = 1e9
        )
      )))
      ok <- !is.na(r$taxonID) & r$taxonID != ""
      r <- r[ok, ]
      # WFO.one() chooses one record per query, preferring the best author match
      # (Auth.dist); it expects the columns that WFO.match() adds only when it
      # replaces synonyms by accepted names
      r$New.accepted <- FALSE
      r$Old.status <- ""
      r$Old.ID <- ""
      utils::capture.output(one <- WorldFlora::WFO.one(
        r,
        Auth.dist = "Auth.dist", verbose = FALSE, counter = 1e9
      ))
      data.frame(
        query = r$key, id = r$taxonID,
        pick = paste(r$key, r$taxonID) %in% paste(one$key, one$taxonID),
        stringsAsFactors = FALSE
      )
    }
  )
}

# ---- timing ----
# Elapsed seconds (best of `reps`) to match `n` queries, after a warm-up call.
# The queries are shuffled (fixed seed) so that every size is a mix of the
# kinds of error: the query set is ordered by kind, and exact names are much
# cheaper to match than damaged ones.
time_adapter <- function(adapter, queries, sizes = c(1, 10, 100, 1000), reps = 3) {
  set.seed(20261109)
  queries <- sample(queries)
  adapter$run(queries[seq_len(min(5, length(queries)))])
  do.call(rbind, lapply(sizes, function(n) {
    q <- rep_len(queries, n)
    q <- unique(q)
    if (length(q) < n) q <- c(q, paste0(q[seq_len(n - length(q))], "x"))
    t <- min(vapply(
      seq_len(reps), function(i) unname(system.time(adapter$run(q))[3]), 0
    ))
    data.frame(n_queries = n, seconds = round(t, 2))
  }))
}
