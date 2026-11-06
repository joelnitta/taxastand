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
#' the number of names is very large. If multiple queries will be made (e.g., to
#' the same large reference database), it is recommended to first parse the
#' names using \code{\link{ts_parse_names}()}, and use the results as input to
#' `query` and/or `reference`.
#'
#' `collapse_infra` is useful in situations where the reference database does
#' not use names that have the same specific epithet and infraspecific epithet.
#' For example, reference name "Blechnum lunare" and query "Blechnum lunare var.
#' lunare". In this case, if `collapse_infra` is `TRUE`, "Blechnum lunare" will
#' be queried instead of "Blechnum lunare var. lunare". Note that the
#' `match_type` will be "exact" even though the literal query and the matched
#' name are different (see example below).
#'
#' @section Match types:
#' The `match_type` column says how a query was matched. Types are tried in
#' the order listed below, and the first one that finds a match is reported, so
#' earlier types are more certain than later ones. Always check results that
#' are not `exact`.
#'
#' **Identical after normalization (safe to accept)**
#' - `exact`: all parts of the name, including the author, are identical.
#' - `auto_punct`: identical after ignoring differences in spaces, periods,
#'   capitalization, and accented or other non-ASCII characters.
#'
#' **Differences in how the author is cited (usually safe)**
#' - `auto_noauth`: query and reference both lack an author and the name is
#'   unique in the reference (only when `match_no_auth = TRUE`).
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
#' - `auto_cfonly`: canonical names are identical. Used automatically for
#'   queries without an author; for queries with an author, only when
#'   `match_canon = TRUE`. Several reference names can share a canonical name
#'   (homonyms), giving multiple hits.
#'
#' **Fuzzy matches (misspellings; always verify)**
#' - `auto_fuzzy`: query has an author; the whole name (author included) is
#'   within `max_dist` of a reference name.
#' - `auto_fuzzy_canon`: query has no author; the canonical names are within
#'   `max_dist`. Which hits are returned is set by `fuzzy_canon_hits`.
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
#' @param reference  Character vector or dataframe; taxonomic names to use as
#' reference. If a character vector, missing values not allowed and all values
#' must be unique. If a dataframe, should be taxonomic names parsed with
#' \code{\link{ts_parse_names}()}.
#' @param manual_match Optional. Dataframe of manually matched names that will
#' override any results from the name-matching engine. Must include columns,
#' `query` and `match`. Can only be used if `query` is a character vector.
#' @param max_dist Max Levenshtein distance to allow during fuzzy matching
#' (total insertions, deletions and substitutions). The distance is computed on
#' the whole name, including the author string, after removing punctuation and
#' converting to lower case. Default: 3.
#' @param match_no_auth Logical; If no author is given in the query and the name
#' (without author) occurs only once in the reference, accept the name in the
#' reference as a match. Default: to not allow such a match (`FALSE`).
#' @param match_canon Logical; Allow a "canonical name" match if only the genus,
#' species epithet, and infraspecific epithet (if present) match exactly.
#' Default: to not allow such a match (`FALSE`).
#' @param collapse_infra Logical; if the specific epithet and infraspecific
#' epithet are the same, drop the infraspecific rank and epithet from the query.
#' @param collapse_infra_exclude Character vector; taxonomic names to exclude
#' from collapsing with `collapse_infra`. Any names used must match those in
#' `query` exactly, or they won't be excluded.
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
#'
#' @param fuzzy_canon_hits Either `"all"` (default) or `"closest"`. Controls
#' which hits are returned when a query *without an author* has no identical
#' canonical name in the reference and is fuzzy-matched on its canonical name
#' (match type `auto_fuzzy_canon`). `"all"` returns every reference name within
#' `max_dist`, which is the most informative but can include many unrelated
#' names when epithets are short. `"closest"` returns only the names at the
#' smallest distance found (ties are all kept, so real ambiguity still shows).
#' @return Dataframe with the following columns (if `simple` is `FALSE`):
#' - query: Query name
#' - reference: Matched reference name
#' - match_type: Type of match (see the "Match types" section)
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
#' @autoglobal
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
#' # Example using collapse_infra argument
#' ts_match_names(
#'   c(
#'     "Crepidomanes minutus", "Blechnum lunare var. lunare",
#'     "Blechnum lunare", "Bar foo var. foo", "Bar foo"
#'   ),
#'   c(
#'     "Crepidomanes minutum", "Hymenophyllum polyanthos", "Blechnum lunare",
#'     "Bar foo"
#'   ),
#'   collapse_infra = TRUE,
#'   collapse_infra_exclude = "Bar foo var. foo",
#'   simple = TRUE
#' )
#'
ts_match_names <- function(
  query,
  reference,
  manual_match = NULL,
  max_dist = 3,
  match_no_auth = FALSE,
  match_canon = FALSE,
  collapse_infra = FALSE,
  collapse_infra_exclude = NULL,
  simple = FALSE,
  tbl_out = getOption("ts_tbl_out", default = FALSE),
  strip_qualifiers = FALSE,
  fuzzy_canon_hits = c("all", "closest")
) {
  # Check input
  assertthat::assert_that(
    is.character(query) | inherits(query, "data.frame"),
    msg = "query must be of class 'data.frame' or a character vector"
  )
  assertthat::assert_that(
    is.character(reference) | inherits(reference, "data.frame"),
    msg = "reference must be of class 'data.frame' or a character vector"
  )
  assertthat::assert_that(assertthat::is.number(max_dist))
  assertthat::assert_that(is.logical(match_no_auth))
  assertthat::assert_that(is.logical(match_canon))
  assertthat::assert_that(is.logical(simple))
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  assertthat::assert_that(assertthat::is.flag(collapse_infra))
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))
  fuzzy_canon_hits <- match.arg(fuzzy_canon_hits)
  if (!is.null(collapse_infra_exclude)) {
    assertthat::assert_that(is.character(collapse_infra_exclude))
  }
  if (!is.null(manual_match)) {
    assertthat::assert_that(
      isTRUE(inherits(manual_match, "data.frame")),
      msg = "manual_match must be of class 'data.frame'"
    )
    assertthat::assert_that(
      isTRUE(
        all(c("query", "match") %in% colnames(manual_match))
      ),
      msg = "manual_match must have `query` and `match` columns"
    )
    assertthat::assert_that(
      is.character(manual_match$query)
    )
    assertthat::assert_that(
      is.character(manual_match$match)
    )
    assertthat::assert_that(
      assertthat::noNA(manual_match$query)
    )
    assertthat::assert_that(
      assertthat::noNA(manual_match$query)
    )
    assertthat::assert_that(
      !anyDuplicated(manual_match$query),
      msg = "All values of manual_match$query must be unique"
    )
    assertthat::assert_that(
      is.character(query),
      msg = "manual_match can only be used if query is a character vector"
    )
  }

  # Helper function to add a namestring to a dataframe of parsed names
  add_namestring <- function(df) {
    df$namestring <-
      paste(
        df$genus_hybrid_sign,
        df$genus_name,
        df$species_hybrid_sign,
        df$specific_epithet,
        df$infraspecific_rank,
        df$infraspecific_epithet,
        df$author,
        sep = "_"
      )
    df
  }

  # Parse or load query names
  if (is.character(query)) {
    # Optional: for manual matches, use matched name instead of query
    # to generate exact match
    if (!is.null(manual_match)) {
      manual_replacement_df <-
        data.frame(
          query_original = query
        ) |>
        dplyr::left_join(
          dplyr::select(
            manual_match,
            query_original = query,
            query_new = match
          ),
          by = "query_original",
          relationship = "one-to-one"
        ) |>
        dplyr::mutate(
          query_new = dplyr::coalesce(query_new, query_original)
        )
      query <- manual_replacement_df$query_new |>
        unique()
    }
    # Parse the names (adds 'name' column)
    query_parsed_df <- ts_parse_names(
      query,
      strip_qualifiers = strip_qualifiers
    )
  } else {
    # Or, names are already parsed
    query_parsed_df <- query
  }

  # Optionally collapse infraspecific name
  if (isTRUE(collapse_infra)) {
    # Save a copy of original unmodified parsed query
    query_parsed_df_original <- query_parsed_df
    # Identify rows where infraspecific_epithet is the same as specific_epithet
    query_parsed_df$same_infra_species <-
      (query_parsed_df$specific_epithet ==
        query_parsed_df$infraspecific_epithet) %in%
        TRUE &
        !query_parsed_df$name %in% collapse_infra_exclude
    assertthat::assert_that(!anyNA(query_parsed_df$same_infra_species))
    # For rows where infraspecific_epithet is the same as specific_epithet,
    # delete infraspecific_epithet and infraspecific_rank
    query_parsed_df$infraspecific_epithet[
      query_parsed_df$same_infra_species
    ] <- NA
    query_parsed_df$infraspecific_rank[query_parsed_df$same_infra_species] <- NA
    query_parsed_df$same_infra_species <- NULL
    # Account for duplicates created after collapsing names: drop them
    query_parsed_df <- add_namestring(query_parsed_df) |>
      dplyr::group_by(namestring) |>
      dplyr::mutate(key_id = dplyr::first(id)) |>
      dplyr::ungroup()
    id_map <- dplyr::select(query_parsed_df, id_query = key_id, id)
    query_parsed_df <- query_parsed_df[
      !duplicated(query_parsed_df$namestring),
    ]
    query_parsed_df$namestring <- NULL
  }

  # Write out parsed names to temporary file
  query_parsed_txt <- tempfile(
    pattern = digest::digest(query),
    fileext = ".txt"
  )
  if (fs::file_exists(query_parsed_txt)) fs::file_delete(query_parsed_txt)
  ts_write_names(query_parsed_df, query_parsed_txt)

  # Only reference names in a query genus can match (see R/genus_filter.R), so
  # drop the rest before the (expensive) parsing and key building
  keep_keys <- .ts_genus_keep_keys(query_parsed_df)

  # Parse or load reference names
  if (is.character(reference)) {
    # Parse the names (adds 'name' column)
    reference <- .ts_filter_raw_reference(reference, keep_keys)
    ref_parsed_df <- if (length(reference)) {
      ts_parse_names(reference, strip_qualifiers = strip_qualifiers)
    } else {
      .ts_empty_parsed_df()
    }
    # IDs from ts_parse_names() are a hash of the input plus a counter, so a
    # reference vector identical to the query vector would get identical IDs
    # and every pair would be mistaken for a self-match. Keep them distinct.
    if (any(ref_parsed_df$id %in% query_parsed_df$id)) {
      ref_parsed_df$id <- paste0("ref-", ref_parsed_df$id)[
        seq_len(nrow(ref_parsed_df))
      ]
    }
  } else {
    # Or, names are already parsed
    ref_parsed_df <- .ts_filter_parsed_reference(reference, keep_keys)
  }

  # Check that manually matched ref names are in data
  if (!is.null(manual_match)) {
    assertthat::assert_that(
      isTRUE(all(manual_match$match %in% ref_parsed_df$name)),
      msg = "One or more manually matched reference names not in reference data"
    )
  }

  # Write out parsed names to temporary file
  ref_parsed_txt <- tempfile(
    pattern = digest::digest(reference),
    fileext = ".txt"
  )
  if (fs::file_exists(ref_parsed_txt)) fs::file_delete(ref_parsed_txt)
  ts_write_names(ref_parsed_df, ref_parsed_txt)

  # Run matching
  results <- tt_matchnames(
    query_lines = readLines(query_parsed_txt),
    ref_lines = readLines(ref_parsed_txt),
    max_dist = max_dist,
    match_no_auth = match_no_auth,
    match_canon = match_canon,
    canon_closest = identical(fuzzy_canon_hits, "closest")
  )

  # `results` has one row per match (17 columns): id_query, id_ref, match_type,
  # then the parsed elements of the query name (genus hybrid sign, genus,
  # species hybrid sign, specific epithet, infraspecific rank, infraspecific
  # epithet, author), then the same for the reference name.

  # Add back in the original search terms (query and reference)
  results <- dplyr::left_join(
    results,
    dplyr::select(query_parsed_df, id_query = id, query = name),
    by = "id_query"
  )

  results <- dplyr::left_join(
    results,
    dplyr::select(ref_parsed_df, id_ref = id, reference = name),
    by = "id_ref"
  )

  results <- dplyr::select(
    results,
    query,
    reference,
    match_type,
    dplyr::everything()
  )

  # Add back in names that were duplicated due to collapsed infrasp names
  if (isTRUE(collapse_infra)) {
    results <-
      dplyr::select(
        query_parsed_df_original,
        query = name,
        id
      ) |>
      dplyr::left_join(id_map, by = "id") |>
      dplyr::left_join(
        dplyr::select(results, -query),
        by = "id_query"
      ) |>
      dplyr::select(-id) |>
      dplyr::select(query, reference, match_type, dplyr::everything())
  }

  # For manual matches, restore back to original query input, and specify
  # that match was made manually
  if (!is.null(manual_match)) {
    results <-
      manual_replacement_df |>
      dplyr::inner_join(
        results,
        by = dplyr::join_by(query_new == query)
      ) |>
      dplyr::mutate(
        match_type = dplyr::case_when(
          query_original %in% manual_match$query ~ "manual",
          .default = match_type
        )
      ) |>
      dplyr::select(
        query = query_original,
        dplyr::everything()
      )
  }

  if (simple) {
    results <- dplyr::select(results, query, reference, match_type)
  }

  if (isTRUE(tbl_out)) {
    return(tibble::as_tibble(results))
  }

  results
}
