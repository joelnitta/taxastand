#' Match taxonomic names to a reference
#'
#' Allows for orthographic differences between query and reference by using
#' fuzzy matching on parsed taxonomic names.
#'
#' Names are matched in two steps:
#' 1. Scientific names are parsed into their component parts (genus, species,
#' variety, author, etc).
#' 2. Names are fuzzily matched following taxonomic rules using the component
#' parts.
#'
#' Parsing is fairly fast (much faster than matching) but can take some time if
#' the number of names is very large. If multiple queries will be made against
#' the same large reference (e.g., a database such as World Flora Online), it
#' is recommended to prepare the reference once with
#' \code{\link{ts_optimize_ref}()} and use the result as `reference`. That
#' saves parsing the reference and building its match keys on every call, so it
#' is the faster option. Alternatively, parse the names first using
#' \code{\link{ts_parse_names}()}, and use the results as input to `query`
#' and/or `reference`.
#'
#' `match_autonyms` is useful when the query and the reference do not agree on
#' whether to use names that have the same specific epithet and infraspecific
#' epithet (autonyms).
#'
#' @section Match types:
#' The `match_type` column says how a query was matched. Types are tried in
#' the order listed below, and the first one that finds a match is reported, so
#' earlier types are more certain than later ones. Always check results that
#' are not `exact`. If several reference records are equally good at the
#' first type that matches (for example, two names that differ only in an *ex*
#' author), every one of them is returned, so the ambiguity is visible.
#'
#' **Identical after normalization (safe to accept)**
#' - `exact`: all parts of the name, including the author, are identical.
#' - `autonym`: identical once an autonym (e.g. "Blechnum lunare var. lunare")
#'   is collapsed to the species name, "Blechnum lunare" (only when
#'   `match_autonyms` is `TRUE`).
#' - `auto_punct`: identical after ignoring differences in spaces, periods,
#'   capitalization, and accented or other non-ASCII characters.
#'
#' **Differences in how the author is cited (usually safe)**
#' - `auto_basio-`, `auto_basio+`: identical once the basionym author, e.g.
#'   `(Turcz.)` in `Cardaminopsis umbrosa (Turcz.) Czerep.`, is dropped from
#'   the reference (`-`) or the query (`+`).
#' - `auto_in-`, `auto_in+`: identical once "in" elements (`Tagawa in Morton`
#'   becomes `Tagawa`) are dropped from the reference or the query.
#' - `auto_ex-`, `auto_ex+`: as above, dropping both "in" and "ex" elements
#'   (`Willd. ex Schkuhr` becomes `Willd.`).
#' - `auto_basexin`: identical once basionym authors and "in"/"ex" elements are
#'   dropped from both query and reference.
#' - `auto_irank`: identical except for the infraspecific rank (e.g. `var.`
#'   vs. `subsp.`).
#'
#' **Canonical-name matches (the author is ignored; check the result)**
#'
#' The canonical name is genus + specific epithet + infraspecific epithet.
#' - `auto_canon`: canonical names are identical. Used automatically for
#'   queries without an author; for queries with an author, only when
#'   `match_canon = TRUE`. Several reference names can share a canonical name
#'   (homonyms), giving multiple hits.
#'
#' **Latin spelling variants (check the result)**
#' - `auto_orthvar`: identical once standard Latin orthographic variants in the
#'   epithets are folded together: `ae`/`oe` = `e`, `y` = `i`, `ph` = `f`,
#'   `rh` = `r`, `th` = `t`, and a final `ii` = `i` (e.g. `sylvestris` and
#'   `silvestris`, `caerulea` and `coerulea`). For queries without an author,
#'   canonical names are compared. This can occasionally merge epithets that
#'   are genuinely different.
#'
#' **Fuzzy matches (misspellings; always verify)**
#' - `auto_fuzzy`: query has an author; the whole name (author included) is
#'   within `max_dist` of a reference name.
#' - `auto_fuzzy_canon`: query has no author; the canonical names are within
#'   `max_dist`.
#'
#' **Fuzzy genus matches (misspelled genus; always verify)**
#' - `auto_fuzzy_genus`: the genus of the query is not in the reference, but is
#'   within `max_dist_genus` of a reference genus, and the rest of the name then
#'   matches within `max_dist` (the total distance includes the genus edits).
#'
#' **No match**
#' - `no_match`: nothing in the reference was close enough.
#'
#' Fuzzy matching is computed with the
#' [Levenshtein distance](https://en.wikipedia.org/wiki/Levenshtein_distance)
#' (total insertions, deletions, and substitutions). The rules for authorship
#' fit plants, algae, and fungi, where the basionym author is given in
#' parentheses and the year is usually omitted; animal names, which normally
#' include the year, may match less well.
#'
#' @param query Character vector or dataframe; taxonomic names to be queried.
#' If a character vector, missing values not allowed and all values must be
#' unique.
#' If a dataframe, should be taxonomic names parsed with
#' \code{\link{ts_parse_names}()}.
#' @param ref  Character vector or dataframe; taxonomic names to use as
#' reference. If a character vector, missing values not allowed and all values
#' must be unique. If a dataframe, should be taxonomic names parsed with
#' \code{\link{ts_parse_names}()}. Can also be the output of
#' \code{\link{ts_optimize_ref}()}, which saves the work of preparing
#' the reference when matching several times against the same large reference.
#' @param manual_match Optional. Dataframe of manually matched names that will
#' override any results from the name-matching engine. Must include columns,
#' `query` and `match`. The values of `query` are compared with the query names
#' (the `name` column, when `query` is a dataframe).
#' @param max_dist Max Levenshtein distance to allow during fuzzy matching
#' (total insertions, deletions and substitutions). The distance is computed on
#' the whole name, including the author string, after removing punctuation and
#' converting to lower case. Default: 3.
#' @param max_dist_genus Max Levenshtein distance between the genus of a query
#' and a genus in the reference, for correcting misspelled genera. Only used
#' for queries whose genus is not in the reference; the candidates are then the
#' names in reference genera within this distance, and the whole name (or the
#' canonical name, for a query without an author) must still be within
#' `max_dist`. Matches have the type `auto_fuzzy_genus`. Set to 0 to turn
#' genus-level fuzzy matching off. Default: 2.
#' @param transpositions Logical; count a swap of two adjacent letters (e.g.
#' `Qurecus` for `Quercus`, a common typing error) as one edit instead of two
#' when computing fuzzy-match distances (optimal string alignment distance
#' rather than plain Levenshtein distance). Set to `FALSE` for plain
#' Levenshtein distance, which is somewhat faster. Default: `FALSE`.
#' @param match_canon Logical; Allow a "canonical name" match if only the genus,
#' species epithet, and infraspecific epithet (if present) match exactly.
#' Default: to not allow such a match (`FALSE`).
#' @param match_autonyms Logical; match autonyms (names whose specific epithet
#' and infraspecific epithet are the same) with the species name.
#' Default: `FALSE`.
#' @param match_autonyms_exclude Character vector; taxonomic names to exclude
#' from `match_autonyms`. Any names used must match those in `query` exactly, or
#' they won't be excluded.
#' @param reference Alias of `ref`, the name used before taxastand 3.0.0; it
#' will be deprecated in a future version.
#' @param collapse_infra,collapse_infra_exclude Aliases of `match_autonyms` and
#' `match_autonyms_exclude`, kept so that existing code keeps working.
#' @param match_no_auth Alias of `match_canon`, kept so that existing code keeps
#' working. It was an option of earlier versions that `match_canon` now covers;
#' note that `match_canon` also allows canonical-name matches for queries that
#' have an author.
#' @param simple Logical; return the output in a simplified format with only the
#' query name, matched reference name, and match type. Default: `FALSE`.
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#' @param strip_qualifiers Logical; if `TRUE`, remove standalone
#' open-nomenclature qualifiers (`cf.`, `aff.`, `s.l.`/`sensu lato`,
#' `s.str.`/`sensu stricto`) from names before parsing. See
#' \code{\link{ts_parse_names}()} and its References section. Only has an
#' effect when `query`/`reference` is a character vector; ignored if
#' already-parsed data.frames are passed in. Default: `FALSE`.
#' @param exclude_placeholders Logical; exclude placeholder names such as
#' `"Genus sp."` or `"Genus indet."` (identification stopped above species
#' rank) with a warning, instead of matching them as if they were species. See
#' \code{\link{ts_parse_names}()}. Only has an effect when `query` is a
#' character vector. Default: `TRUE`.
#' @return Dataframe with the following columns (if `simple` is `FALSE`):
#' - query: Query name
#' - reference: Matched reference name
#' - match_type: Type of match (see the "Match types" section)
#' - dist: Edit distance between query and reference for fuzzy matches
#'   (`auto_fuzzy`, `auto_fuzzy_canon`, `auto_fuzzy_genus`), `NA` otherwise
#' - id_query: Unique ID of query
#' - id_ref: Unique ID of reference
#' - genus_hybrid_sign_query: Genus hybrid sign in query
#' - genus_name_query: Genus name of query
#' - species_hybrid_sign_query: Species hybrid sign in query
#' - specific_epithet_query: Specific epithet of query
#' - infraspecific_rank_query: Infraspecific rank of query
#' - infraspecific_epithet_query: Infraspecific epithet of query
#' - author_query: Taxonomic author of query
#' - genus_hybrid_sign_ref: Genus hybrid sign in reference
#' - genus_name_ref: Genus name of reference
#' - species_hybrid_sign_ref: Species hybrid sign in reference
#' - specific_epithet_ref: Specific epithet of reference
#' - infraspecific_rank_ref: Infraspecific rank of reference
#' - infraspecific_epithet_ref: Infraspecific epithet of reference
#' - author_ref: Taxonomic author of reference
#'
#' If `simple` is `TRUE`, only return the first three columns above.
#'
#' @references Sigovini, M., Keppel, E., & Tagliapietra, D. (2016). Open
#' Nomenclature in the biodiversity era. Methods in Ecology and Evolution,
#' 7(10), 1217-1225. \doi{10.1111/2041-210X.12594}
#'
#' @export
#' @examples
#' ts_match_names(
#'   "Crepidomanes minutus",
#'   c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'   simple = TRUE
#' )
#'
#' # If names are too distant, they won't match
#' ts_match_names(
#'   query = "Crepidblah foo",
#'   reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'   simple = TRUE
#' )
#'
#' # But we can force a match manually
#' ts_match_names(
#'   query = "Crepidblah foo",
#'   reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'   manual_match = data.frame(
#'     query = c("Crepidblah foo"),
#'     match = c("Crepidomanes minutum")
#'   ),
#'   simple = TRUE
#' )
#'
#' # If you always want tibble output without specifying `tbl_out = TRUE`
#' # every time, set the option:
#' options(ts_tbl_out = TRUE)
#' ts_match_names(
#'   "Crepidomanes minutus",
#'   c("Crepidomanes minutum", "Hymenophyllum polyanthos")
#' )
#'
#' # Example using the match_autonyms argument
#' ts_match_names(
#'   c(
#'     "Crepidomanes minutus", "Blechnum lunare var. lunare",
#'     "Blechnum lunare", "Bar foo var. foo", "Bar foo"
#'   ),
#'   c(
#'     "Crepidomanes minutum", "Hymenophyllum polyanthos", "Blechnum lunare",
#'     "Bar foo"
#'   ),
#'   match_autonyms = TRUE,
#'   match_autonyms_exclude = "Bar foo var. foo",
#'   simple = TRUE
#' )
#'
ts_match_names <- function(
  query,
  ref,
  manual_match = NULL,
  max_dist = 3,
  max_dist_genus = 2,
  transpositions = FALSE,
  match_canon = FALSE,
  match_autonyms = FALSE,
  match_autonyms_exclude = NULL,
  simple = FALSE,
  tbl_out = getOption("ts_tbl_out", default = FALSE),
  strip_qualifiers = FALSE,
  exclude_placeholders = TRUE,
  collapse_infra = NULL,
  collapse_infra_exclude = NULL,
  reference = NULL,
  match_no_auth = NULL
) {
  match_canon <- .ts_alias_match_no_auth(match_canon, match_no_auth, missing(match_canon))
  ref <- .ts_alias(ref, reference, missing(ref), "ref", "reference")
  match_autonyms <- .ts_alias(
    match_autonyms, collapse_infra, missing(match_autonyms),
    "match_autonyms", "collapse_infra"
  )
  match_autonyms_exclude <- .ts_alias(
    match_autonyms_exclude, collapse_infra_exclude,
    missing(match_autonyms_exclude),
    "match_autonyms_exclude", "collapse_infra_exclude"
  )
  .ts_check_match_args(
    query, ref, manual_match, max_dist, max_dist_genus, transpositions,
    match_canon, simple, tbl_out, match_autonyms,
    match_autonyms_exclude, strip_qualifiers
  )

  # Parse or load query names. For manual matches, the matched name is used
  # instead of the query to generate an exact match.
  manual_replacement_df <- NULL
  if (is.character(query)) {
    if (!is.null(manual_match)) {
      manual_replacement_df <- .ts_manual_replacement(query, manual_match)
      query <- unique(manual_replacement_df$query_new)
    }
    query_parsed_df <- ts_parse_names(
      query,
      strip_qualifiers = strip_qualifiers,
      exclude_placeholders = exclude_placeholders
    )
  } else {
    query_parsed_df <- query # already parsed
    if (!is.null(manual_match)) {
      query_parsed_df <- .ts_manual_parsed(
        query_parsed_df, manual_match, strip_qualifiers, exclude_placeholders
      )
    }
  }

  # Optionally collapse infraspecific name
  collapsed <- NULL
  if (isTRUE(match_autonyms)) {
    collapsed <- .ts_collapse_infra(query_parsed_df, match_autonyms_exclude)
    query_parsed_df <- collapsed$parsed
  }

  ref_prep <- .ts_prepare_reference(
    ref, query_parsed_df, max_dist_genus, strip_qualifiers
  )

  # With match_autonyms, also ask for the autonyms that the reference has
  variants <- NULL
  if (isTRUE(match_autonyms)) {
    variants <- .ts_autonym_variants(
      collapsed, match_autonyms_exclude, .ts_ref_autonym_keys(ref_prep)
    )
  }

  # Check that manually matched ref names are in data
  if (!is.null(manual_match)) {
    assertthat::assert_that(
      isTRUE(all(manual_match$match %in% ref_prep$names)),
      msg = "One or more manually matched reference names not in reference data"
    )
  }

  # Run matching (one row per match; see .tt_assemble() for the columns)
  results <- tt_matchnames(
    query_lines = .ts_parsed_lines(
      if (is.null(variants)) query_parsed_df else rbind(query_parsed_df, variants$parsed),
      query
    ),
    ref_lines = if (is.null(ref_prep$keys)) {
      .ts_parsed_lines(ref_prep$parsed, ref)
    },
    ref_keys = ref_prep$keys,
    max_dist = max_dist,
    match_canon = match_canon,
    max_dist_genus = max_dist_genus,
    transpositions = transpositions
  )

  if (!is.null(variants)) {
    results <- .ts_use_autonym_variants(results, variants, query_parsed_df$id)
  }

  # Add back in the original search terms (query and reference)
  results$query <-
    query_parsed_df$name[match(results$id_query, query_parsed_df$id)]
  results$reference <- ref_prep$names[match(results$id_ref, ref_prep$ids)]
  results <- .ts_front_cols(results)

  # Add back in names that were duplicated due to collapsed infrasp names
  if (!is.null(collapsed)) {
    results <- .ts_expand_collapsed(results, collapsed)
  }

  # For manual matches, restore back to original query input, and specify
  # that match was made manually
  if (!is.null(manual_match)) {
    results <- if (is.null(manual_replacement_df)) {
      .ts_mark_manual(results, manual_match)
    } else {
      .ts_restore_manual(results, manual_replacement_df, manual_match)
    }
  }

  if (simple) {
    results <- results[, c("query", "reference", "match_type")]
  }

  if (isTRUE(tbl_out)) {
    return(.ts_as_tibble(results))
  }

  results
}

# ---- Helpers for ts_match_names() --------------------------------------------

# Validate the arguments of ts_match_names()
.ts_check_match_args <- function(
  query, reference, manual_match, max_dist, max_dist_genus, transpositions,
  match_canon, simple, tbl_out, match_autonyms,
  match_autonyms_exclude, strip_qualifiers
) {
  assertthat::assert_that(
    is.character(query) | inherits(query, "data.frame"),
    msg = "query must be of class 'data.frame' or a character vector"
  )
  assertthat::assert_that(
    is.character(reference) | inherits(reference, "data.frame") |
      inherits(reference, "ts_reference"),
    msg = paste(
      "ref must be of class 'data.frame', a character vector,",
      "or the output of ts_optimize_ref()"
    )
  )
  assertthat::assert_that(assertthat::is.number(max_dist))
  assertthat::assert_that(assertthat::is.number(max_dist_genus))
  assertthat::assert_that(assertthat::is.flag(transpositions))
  assertthat::assert_that(is.logical(match_canon))
  assertthat::assert_that(is.logical(simple))
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  assertthat::assert_that(assertthat::is.flag(match_autonyms))
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))
  if (!is.null(match_autonyms_exclude)) {
    assertthat::assert_that(is.character(match_autonyms_exclude))
  }
  if (!is.null(manual_match)) {
    assertthat::assert_that(
      isTRUE(inherits(manual_match, "data.frame")),
      msg = "manual_match must be of class 'data.frame'"
    )
    assertthat::assert_that(
      isTRUE(all(c("query", "match") %in% colnames(manual_match))),
      msg = "manual_match must have `query` and `match` columns"
    )
    assertthat::assert_that(is.character(manual_match$query))
    assertthat::assert_that(is.character(manual_match$match))
    assertthat::assert_that(assertthat::noNA(manual_match$query))
    assertthat::assert_that(
      !anyDuplicated(manual_match$query),
      msg = "All values of manual_match$query must be unique"
    )
  }
  invisible(TRUE)
}

# Data frame of each query and the name to match in its place (the manually
# matched name if there is one, else the query itself)
.ts_manual_replacement <- function(query, manual_match) {
  out <- data.frame(query_original = query)
  hit <- match(query, manual_match$query)
  out$query_new <- ifelse(is.na(hit), query, manual_match$match[hit])
  out
}

# Parsed (dataframe) query: for the rows named in manual_match, swap the parsed
# name parts for those of the manually matched name, keeping the row's name and
# id, so that the engine finds an exact match (as for a character query)
.ts_manual_parsed <- function(parsed, manual_match, strip_qualifiers,
                              exclude_placeholders) {
  hit <- match(parsed$name, manual_match$query)
  rows <- which(!is.na(hit))
  if (!length(rows)) {
    return(parsed)
  }
  new_names <- manual_match$match[hit[rows]]
  not_parsed <- "One or more manually matched names could not be parsed"
  repl <- tryCatch(
    suppressWarnings(ts_parse_names(
      unique(new_names),
      strip_qualifiers = strip_qualifiers,
      exclude_placeholders = exclude_placeholders
    )),
    error = function(e) stop(not_parsed, call. = FALSE)
  )
  repl <- repl[match(new_names, repl$name), , drop = FALSE]
  assertthat::assert_that(!anyNA(repl$name), msg = not_parsed)
  parts <- intersect(setdiff(colnames(parsed), c("name", "id")), colnames(repl))
  parsed[rows, parts] <- repl[, parts]
  parsed
}

# Collapse the infraspecific epithet of queries where it is the same as the
# specific epithet (except for names in `exclude`), and drop the duplicate names
# this creates. Returns the reduced parsed names, the unmodified ones, and a map
# from the id of each dropped name to the id of the first identical one.
.ts_collapse_infra <- function(parsed, exclude) {
  original <- parsed
  same <- (parsed$specific_epithet == parsed$infraspecific_epithet) %in% TRUE &
    !parsed$name %in% exclude
  assertthat::assert_that(!anyNA(same))
  parsed$infraspecific_epithet[same] <- NA
  parsed$infraspecific_rank[same] <- NA
  namestring <- do.call(
    paste,
    c(
      unname(as.list(parsed[, c(
        "genus_hybrid_sign", "genus_name", "species_hybrid_sign",
        "specific_epithet", "infraspecific_rank", "infraspecific_epithet",
        "author"
      )])),
      sep = "_"
    )
  )
  key_id <- parsed$id[match(namestring, namestring)]
  list(
    parsed = parsed[!duplicated(namestring), ],
    original = original,
    id_map = data.frame(id_query = key_id, id = parsed$id),
    autonym_ids = original$id[same]
  )
}

# Extra queries for match_autonyms, so that an autonym in the reference can be
# found: a species-level query is also asked as an autonym ("Blechnum lunare"
# as "Blechnum lunare var. lunare"), and an autonym that was collapsed is also
# asked as it was written. Only those whose genus and species are an autonym in
# the reference (`ref_keys`) are kept. `base_id` is the id of the query each one
# stands for.
.ts_autonym_variants <- function(collapsed, exclude, ref_keys) {
  kept <- collapsed$parsed
  orig <- collapsed$original[match(kept$id, collapsed$original$id), ]
  was_autonym <- kept$id %in% collapsed$autonym_ids
  species_level <- !was_autonym & is.na(kept$infraspecific_epithet) &
    !is.na(kept$specific_epithet) & !kept$name %in% exclude
  literal <- orig[was_autonym, ]
  constructed <- kept[species_level, ]
  constructed$infraspecific_rank <- rep("var.", nrow(constructed))
  constructed$infraspecific_epithet <- constructed$specific_epithet
  parsed <- rbind(literal, constructed)
  base_id <- c(kept$id[was_autonym], kept$id[species_level])
  is_literal <- c(rep(TRUE, nrow(literal)), rep(FALSE, nrow(constructed)))
  keep <- .ts_autonym_key(parsed$genus_name, parsed$specific_epithet) %in%
    ref_keys
  if (!any(keep)) {
    return(NULL)
  }
  parsed <- parsed[keep, ]
  parsed$id <- paste0(base_id[keep], "-autonym")
  list(parsed = parsed, base_id = base_id[keep], literal = is_literal[keep])
}

# Key of a genus and specific epithet that is the same for spellings that
# differ only in punctuation, case or Latin orthographic variants
.ts_autonym_key <- function(genus, epithet) {
  genus[is.na(genus)] <- ""
  epithet[is.na(epithet)] <- ""
  paste0(tt_depunct(genus), .tt_orthfold(tt_depunct(epithet)))
}

# Keys (see .ts_autonym_key()) of the genus and species of the autonyms in the
# reference
.ts_ref_autonym_keys <- function(ref_prep) {
  if (is.null(ref_prep$keys)) {
    p <- ref_prep$parsed
    genus <- p$genus_name
    epithet <- p$specific_epithet
    infra <- p$infraspecific_epithet
  } else {
    genus <- ref_prep$keys$fields$g
    epithet <- ref_prep$keys$fields$s
    infra <- ref_prep$keys$fields$ssp
  }
  infra[is.na(infra)] <- ""
  epithet[is.na(epithet)] <- ""
  same <- nzchar(infra) &
    .tt_orthfold(tt_depunct(epithet)) == .tt_orthfold(tt_depunct(infra))
  unique(.ts_autonym_key(genus[same], epithet[same]))
}

# Use the matches of the autonym variants for the queries that otherwise have
# no match, or only fuzzy matches. A constructed variant that matches exactly
# (or differs only in rank) is an `autonym` match; other match types are kept.
.ts_use_autonym_variants <- function(results, variants, query_ids) {
  is_variant <- results$id_query %in% variants$parsed$id
  base <- results[!is_variant, ]
  hits <- results[is_variant, ]
  hits <- hits[hits$match_type != "no_match" &
    !grepl("^auto_fuzzy", hits$match_type), ]
  if (nrow(hits) == 0) {
    return(base)
  }
  i <- match(hits$id_query, variants$parsed$id)
  hits$id_query <- variants$base_id[i]
  hits$match_type[!variants$literal[i] &
    hits$match_type %in% c("exact", "auto_irank")] <- "autonym"
  unresolved <- vapply(
    split(base$match_type, base$id_query),
    function(x) all(x == "no_match" | grepl("^auto_fuzzy", x)),
    logical(1)
  )
  hits <- hits[hits$id_query %in% names(unresolved)[unresolved], ]
  if (nrow(hits) == 0) {
    return(base)
  }
  # the variant's own query fields are replaced by those of the query
  query_cols <- setdiff(grep("_query$", names(results), value = TRUE), "id_query")
  hits[query_cols] <- base[match(hits$id_query, base$id_query), query_cols]
  out <- rbind(base[!base$id_query %in% hits$id_query, ], hits)
  out <- out[order(match(out$id_query, query_ids)), ]
  rownames(out) <- NULL
  out
}

# Parsed reference names (and, when available, their match keys), restricted to
# those that could match a query genus (see R/genus_filter.R) before the
# expensive parsing and key building. A character vector is wrapped in a
# temporary prepared reference, so there is a single code path; a prepared
# reference keeps what it has built for later calls. Returns the `ids` and
# `names` of the reference records, and their match `keys`; for a reference that
# was already parsed (a data frame of parsed names) `keys` is NULL and the
# records are returned as `parsed`.
.ts_prepare_reference <- function(
  reference, query_parsed_df, max_dist_genus, strip_qualifiers
) {
  if (inherits(reference, "data.frame")) {
    keep_keys <- .ts_genus_keep_keys(
      query_parsed_df, tt_depunct(reference$genus_name), max_dist_genus
    )
    parsed <- .ts_filter_parsed_reference(reference, keep_keys)
    return(list(
      ids = parsed$id, names = parsed$name, parsed = parsed, keys = NULL
    ))
  }
  if (!inherits(reference, "ts_reference")) {
    reference <- .ts_new_reference(reference, strip_qualifiers)
  }
  .ts_ref_subset(reference, query_parsed_df, max_dist_genus)
}

# Parsed names as the pipe-delimited lines the matching engine reads (written
# via a temporary file, as ts_write_names() does)
.ts_parsed_lines <- function(parsed_df, key) {
  path <- tempfile(pattern = digest::digest(key), fileext = ".txt")
  on.exit(unlink(path), add = TRUE)
  ts_write_names(parsed_df, path)
  readLines(path)
}

# Put the main result columns first
.ts_front_cols <- function(df) {
  front <- c("query", "reference", "match_type", "dist")
  df[, c(front, setdiff(colnames(df), front))]
}

# Give every original query its results back, including the ones dropped as
# duplicates when infraspecific epithets were collapsed
.ts_expand_collapsed <- function(results, collapsed) {
  results_unexpanded <- results[, setdiff(colnames(results), "query")]
  out <- data.frame(
    query = collapsed$original$name,
    id = collapsed$original$id
  )
  out <- .ts_join(out, collapsed$id_map, "id", "id")
  out <- .ts_join(out, results_unexpanded, "id_query", "id_query")
  # An autonym that matched exactly only after being collapsed is not an exact
  # match of the name that was queried
  out$match_type[out$id %in% collapsed$autonym_ids &
    out$match_type %in% "exact" & is.na(out$infraspecific_epithet_ref)] <- "autonym"
  out$id <- NULL
  .ts_front_cols(out)
}

# Mark the matches of manually matched queries as "manual" (dataframe query)
.ts_mark_manual <- function(results, manual_match) {
  results$match_type[results$query %in% manual_match$query] <- "manual"
  results
}

# Restore the original query input for manual matches and mark those matches
# as "manual"
.ts_restore_manual <- function(results, manual_replacement_df, manual_match) {
  out <- .ts_join(
    manual_replacement_df, results, "query_new", "query",
    inner = TRUE
  )
  out$match_type <- ifelse(
    out$query_original %in% manual_match$query,
    "manual",
    out$match_type
  )
  out$query <- out$query_original
  out$query_original <- NULL
  out[, c("query", setdiff(colnames(out), "query"))]
}
