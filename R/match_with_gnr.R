#' Match names using the Global Names Resolver
#'
#' Matches names using the [Global Names Resolver](https://resolver.globalnames.org/)
#' API. This requires an internet connection. The GNR combines parsing of names
#' with fuzzy matching to match names to a target taxonomic database.
#'
#' Attempts to match hybrid names usually don't work, so any names
#' that appear to by hybrids (e.g., with " x " in the name) are excluded
#'
#' It is not recommended to use more than one data source at a time, as this may
#' complicate resolving synonymy.
#'
#' Requires [gnparser](https://gitlab.com/gogna/gnparser) to be installed.
#'
#' @param names Character vector of names to resolve
#' @param data_source_ids Numeric vector of data source IDs to use for
#' search. Default = 1, the Catalog of Life. For other data sources, see
#' \code{\link[taxize]{gnr_datasources}}
#' @param exclude_mult_matches Logical; should names resulting in multiple
#' matches be excluded as failures? If `TRUE` (the default), the output will have
#' a number of rows equal to length of `names`. If `FALSE`, the output may have
#' more rows than the length of `names`.
#' @param gnparser_path String; path to gnparser executable.
#' Either this must be provided, or gnparser must be on $PATH.
#'
#' @return tibble
#'
#' @examples
#' match_with_gnr(
#'  c(
#'   "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#'   "Crepidomanes minutum var. minutus",
#'   "Hymenophyllum ×polyanthos",
#'   "Acrostichum aureum",
#'   "Cystopteris fragilis",
#'   "fizz bazz")
#' )
#'
#' match_with_gnr(
#'  c(
#'   "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#'   "Crepidomanes minutum var. minutus",
#'   "Hymenophyllum ×polyanthos",
#'   "Acrostichum aureum",
#'   "Cystopteris fragilis",
#'   "fizz bazz"),
#'   exclude_mult_matches = FALSE
#' )
#'
#' @export
match_with_gnr <- function (names, data_source_ids = 1,
                            exclude_mult_matches = TRUE,
                            gnparser_path = NULL) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(is.numeric(data_source_ids))

  names_to_resolve <- names

  # Check for and exclude hybrids from names to resolve
  hybrids_excluded <- tibble::tibble(user_supplied_name = names_to_resolve) %>%
    dplyr::mutate(hybrid = dplyr::case_when(
      stringr::str_detect(user_supplied_name, " x ") ~ TRUE,
      stringr::str_detect(user_supplied_name, "\\u00d7") ~ TRUE, # unicode for batsu
      TRUE ~ FALSE
    )) %>%
    dplyr::mutate(fail_reason = "hybrid excluded") %>%
    dplyr::filter(hybrid == TRUE) %>%
    dplyr::select(-hybrid)

  hybrid_check <- assertthat::validate_that(
    length(hybrids_excluded$user_supplied_name) == 0,
    msg = glue::glue("{length(hybrids_excluded$user_supplied_name)} hybrids detected and excluded"))

  if(is.character(hybrid_check)) print (hybrid_check)

  names_to_resolve <- names_to_resolve[!names_to_resolve %in% hybrids_excluded$user_supplied_name]

  # Parse and match names using Global Name Resolver
  gnr_results_col <- taxize::gnr_resolve(
    names = names_to_resolve,
    data_source_ids = data_source_ids,
    resolve_once = FALSE,
    with_context = FALSE,
    canonical = FALSE,
    best_match_only = FALSE,
    with_canonical_ranks = FALSE,
    fields = "minimal",
    cap_first = FALSE)

  # Early exit if zero results
  if(nrow(gnr_results_col) == 0) return (gnr_results_col)

  # Modify output
  gnr_results_col <-
    gnr_results_col %>%
    add_parsed_names(matched_name, species, taxon, author, gnparser_path = gnparser_path) %>%
    dplyr::rename(matched_species = species, matched_taxon = taxon, matched_author = author) %>%
    # Add lowest taxonomic rank
    dplyr::mutate(lowest_taxonomic_rank = dplyr::case_when(
      # infraspecific includes two spaces
      # (between genus - species and species - infrasp),
      stringr::str_count(matched_taxon, " ") == 2 ~ "infraspecies",
      # but species is only one space
      stringr::str_count(matched_taxon, " ") == 1 ~ "species",
      stringr::str_count(matched_taxon, " ") == 0 ~ "genus",
      stringr::str_count(matched_taxon, " ") > 2 ~ "other"
    ))

  # Drop matches that match to the same taxon except author is missing for one name
  to_pull <-
  gnr_results_col %>%
    dplyr::add_count(matched_taxon) %>%
    dplyr::filter(n > 1) %>%
    dplyr::filter(matched_author == "")

  gnr_results_col <- suppressMessages(dplyr::anti_join(gnr_results_col, to_pull))

  ## Filter results ##

  # Keep GNR results that matched a single species regardless
  # of score
  gnr_results_single_match <-
    gnr_results_col %>%
    dplyr::filter(lowest_taxonomic_rank != "genus") %>%
    dplyr::add_count(user_supplied_name) %>%
    dplyr::filter(n == 1)

  # Also keep GNR results that matched single name
  # with > 0.988 score
  gnr_results_best_match <-
    gnr_results_col %>%
    dplyr::filter(lowest_taxonomic_rank != "genus") %>%
    dplyr::add_count(user_supplied_name) %>%
    dplyr::filter(n > 1) %>%
    dplyr::select(-n) %>%
    dplyr::filter(score > 0.988) %>%
    dplyr::add_count(user_supplied_name) %>%
    dplyr::filter(n == 1)

  # Combine these into GNR results with a match
  gnr_results_with_match <- dplyr::bind_rows(
    gnr_results_single_match,
    gnr_results_best_match)

  # Make tibble of names with multiple fuzzy matches
  multiple_gnr_match <-
    # start with results
    gnr_results_col %>%
    # exclude resolved matched
    dplyr::anti_join(gnr_results_with_match, by = "user_supplied_name") %>%
    # filter to those with >1 match
    dplyr::add_count(user_supplied_name) %>%
    dplyr::filter(n > 1)

  # Make tibble of names with no matches at all
  no_gnr_match <-
    # start with all query names
    tibble::tibble(user_supplied_name = names) %>%
    # exclude hybrids
    dplyr::filter(user_supplied_name %in% names_to_resolve) %>%
    # anti-join to remove matched names
    dplyr::anti_join(gnr_results_with_match, by = "user_supplied_name") %>%
    # anti-join to remove multiple matches
    dplyr::anti_join(multiple_gnr_match, by = "user_supplied_name") %>%
    # anything left didn't have a match at all
    unique %>%
    dplyr::mutate(fail_reason = "no match")

  if(exclude_mult_matches) {
    multiple_gnr_match <-
      multiple_gnr_match %>%
      dplyr::select(user_supplied_name) %>%
      unique %>%
      dplyr::mutate(fail_reason = "multiple matches")
  }

  # Combine into final matching output
  dplyr::bind_rows(
    gnr_results_with_match,
    multiple_gnr_match,
    no_gnr_match,
    hybrids_excluded) %>%
    dplyr::select(
      query = user_supplied_name,
      matched_name,
      matched_species,
      matched_taxon,
      rank = lowest_taxonomic_rank,
      score,
      fail_reason
    )

}
