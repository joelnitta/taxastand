#' Match names using the Global Name Resolver
#'
#' Attempts to match hybrid names usually don't work, so any names
#' that appear to by hybrids (e.g., with " x " in the name) are excluded
#'
#' It is not recommended to use more than one data source at a time, and
#' default search values are set with this expectation.
#'
#' Requires [gnparser](https://gitlab.com/gogna/gnparser) to be installed
#' and on the user's PATH.
#'
#' @param names Character vector of names to resolve
#' @param data_source_ids Numeric vector of data source IDs to use for
#' search. Default = 1, the Catalog of Life. For other data sources, see
#' \code{\link[taxize]{gnr_datasources}}
#'
#' @return tibble
#'
#' @examples
#' match_with_gnr(c(
#'   "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#'   "Crepidomanes minutum var. minutus",
#'   "Hymenophyllum ×polyanthos")
#' )
#'
#' @export
match_with_gnr <- function (names, data_source_ids = 1) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(is.numeric(data_source_ids))

  names_to_resolve <- names

  # Check for and exclude hybrids
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

  # Modify output
  gnr_results_col <-
    gnr_results_col %>%
    add_parsed_names(matched_name, matched_species, matched_taxon) %>%
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

  # Also make tibble of names with multiple fuzzy matches
  mult_gnr_match <- gnr_results_col %>%
    dplyr::anti_join(gnr_results_with_match, by = "user_supplied_name") %>%
    dplyr::mutate(fail_reason = "multiple matches") %>%
    dplyr::select(user_supplied_name, fail_reason) %>%
    unique

  # Also make tibble of names with no matches at all
  no_gnr_match <-
    gnr_results_col %>%
    dplyr::filter(!user_supplied_name %in% names_to_resolve) %>%
    dplyr::mutate(fail_reason = "no match") %>%
    dplyr::select(user_supplied_name, fail_reason) %>%
    unique

  # Combine into final matching output
  dplyr::bind_rows(
    gnr_results_with_match,
    mult_gnr_match,
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
