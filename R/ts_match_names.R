#' Match taxonomic names to a reference
#'
#' Allows for orthographic differences between query and reference by using
#' fuzzy matching on parsed taxonomic names. Requires
#' [taxon-tools](https://github.com/camwebb/taxon-tools) to be installed.
#'
#' `taxon-tools` matches names in two steps:
#' 1. Scientific names are parsed into their component parts (genus, species,
#' variety, author, etc).
#' 2. Names are fuzzily matched following taxonomic rules using the component
#' parts.
#'
#' For more information on rules used for matching, [see taxon-tools manual](https://github.com/camwebb/taxon-tools/blob/master/doc/matchnames.md#matching-rules-and-output-codes).
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
#' override any results from `taxon-tools`. Must include columns, `query`
#' and `match`. Can only be used if `query` is a character vector.
#' @param max_dist Max Levenshtein distance to allow during fuzzy matching
#' (total insertions, deletions and substitutions). Default: 10.
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
#' @param docker Logical; if TRUE, docker will be used to run taxon-tools
#' (so that taxon-tools need not be installed).
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#'
#' @return Dataframe with the following columns (if `simple` is `FALSE`):
#' - query: Query name
#' - reference: Matched reference name
#' - match_type: Type of match (for a summary of match types, [see taxon-tools manual](https://github.com/camwebb/taxon-tools/blob/master/doc/matchnames.md#matching-rules-and-output-codes))
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
#' @autoglobal
#' @export
#' @examples
#' if(ts_tt_installed()) {
#'   ts_match_names(
#'     "Crepidomanes minutus",
#'     c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'     simple = TRUE
#'     )
#'
#'   # If names are too distant, they won't match
#'   ts_match_names(
#'     query = "Crepidblah foo",
#'     reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'     simple = TRUE
#'     )
#'
#'   # But we can force a match manually
#'   ts_match_names(
#'     query = "Crepidblah foo",
#'     reference = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
#'     manual_match = data.frame(
#'       query = c("Crepidblah foo"),
#'       match = c("Crepidomanes minutum")
#'     ),
#'     simple = TRUE
#'    )
#'
#'   # If you always want tibble output without specifying `tbl_out = TRUE`
#'   # every time, set the option:
#'   options(ts_tbl_out = TRUE)
#'   ts_match_names(
#'     "Crepidomanes minutus",
#'     c("Crepidomanes minutum", "Hymenophyllum polyanthos")
#'     )
#'
#'   # Example using collapse_infra argument
#'   ts_match_names(
#'     c("Crepidomanes minutus", "Blechnum lunare var. lunare",
#'       "Blechnum lunare", "Bar foo var. foo", "Bar foo"),
#'     c("Crepidomanes minutum", "Hymenophyllum polyanthos", "Blechnum lunare",
#'       "Bar foo"),
#'     collapse_infra = TRUE,
#'     collapse_infra_exclude = "Bar foo var. foo",
#'     simple = TRUE
#'     )
#' }
#'
ts_match_names <- function(
  query,
  reference,
  manual_match = NULL,
  max_dist = 10,
  match_no_auth = FALSE,
  match_canon = FALSE,
  collapse_infra = FALSE,
  collapse_infra_exclude = NULL,
  simple = FALSE,
  docker = getOption("ts_docker", default = FALSE),
  tbl_out = getOption("ts_tbl_out", default = FALSE)
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
  if (!is.null(collapse_infra_exclude)) {
    assertthat::assert_that(is.character(collapse_infra_exclude))
  }
  assertthat::assert_that(assertthat::is.flag(docker))
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
      isTRUE(!any(duplicated(manual_match$query))),
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
      paste0(
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
    query_parsed_df <- ts_parse_names(query, docker = docker)
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

  # Parse or load reference names
  if (is.character(reference)) {
    # Parse the names (adds 'name' column)
    ref_parsed_df <- ts_parse_names(reference, docker = docker)
  } else {
    # Or, names are already parsed
    ref_parsed_df <- reference
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

  # Format argument flags
  if (match_no_auth) match_no_auth <- "-1" else match_no_auth <- NULL
  if (match_canon) match_canon <- "-c" else match_canon <- NULL

  # Specify temporary output file
  match_results_txt <- tempfile(
    pattern = digest::digest(c(query, reference)),
    fileext = ".txt"
  )
  if (fs::file_exists(match_results_txt)) fs::file_delete(match_results_txt)

  # Run taxon-tools matchnames

  if (isTRUE(docker)) {
    assertthat::assert_that(
      requireNamespace("babelwhale", quietly = TRUE),
      msg = "babelwhale needs to be installed to use docker"
    )
    assertthat::assert_that(
      babelwhale::test_docker_installation(),
      msg = "docker not installed"
    )
    match_results <- run_auto_mount(
      container_id = "camwebb/taxon-tools:v1.3.0",
      command = "matchnames",
      args = c(
        "-a",
        file = query_parsed_txt,
        "-b",
        file = ref_parsed_txt,
        "-o",
        file = match_results_txt,
        "-e",
        max_dist,
        "-F", # no manual matching
        match_no_auth,
        match_canon
      )
    )
  } else {
    assertthat::assert_that(
      ts_tt_installed(),
      msg = "taxon-tools not installed"
    )
    match_results <- processx::run(
      command = "matchnames",
      args = c(
        "-a",
        query_parsed_txt,
        "-b",
        ref_parsed_txt,
        "-o",
        match_results_txt,
        "-e",
        max_dist,
        "-F", # no manual matching
        match_no_auth,
        match_canon
      )
    )
  }

  # Read in results
  # Each line represents a single name from the query list (list A).
  # Seventeen pipe-delimited (“|”) fields per row:
  #  1. User ID code in list A,
  #  2. Code in list B (if matched),
  #  3. Match type (see codes below),
  #  4-10. Parsed elements of name in list A.
  #  11-17 (in same format as name input), Parsed elements of name in list B.
  matchnames_cols <- c(
    "id_query",
    "id_ref",
    "match_type",
    "genus_hybrid_sign_query",
    "genus_name_query",
    "species_hybrid_sign_query",
    "specific_epithet_query",
    "infraspecific_rank_query",
    "infraspecific_epithet_query",
    "author_query",
    "genus_hybrid_sign_ref",
    "genus_name_ref",
    "species_hybrid_sign_ref",
    "specific_epithet_ref",
    "infraspecific_rank_ref",
    "infraspecific_epithet_ref",
    "author_ref"
  )

  results <- data.frame(record = readLines(match_results_txt))

  results <- tidyr::separate(
    data = results,
    col = record,
    into = matchnames_cols,
    sep = "\\|",
    fill = "right",
    remove = TRUE
  )

  # Convert empty strings to NA
  results <- dplyr::mutate(
    results,
    dplyr::across(dplyr::everything(), ~ dplyr::na_if(.x, ""))
  )

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
      dplyr::left_join(
        results,
        by = dplyr::join_by(query_new == query),
        relationship = "many-to-one"
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

  if (simple == TRUE)
    results <- dplyr::select(results, query, reference, match_type)

  if (isTRUE(tbl_out)) return(tibble::as_tibble(results))

  results
}
