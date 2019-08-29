#' Resolve names based on scientific name, taxon, and species name
#'
#' @param names_to_resolve Dataframe of taxonomic names to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to. Here we use
#' the World Ferns database.
#' @param match_order Vector designating order in which matching should proceed.
#' Names in vector must match column names in `names_to_resolve` used for matching.
#' @param exclude Optional list of taxa vectors to exclude from matches. Names in each vector
#' must match names in `col_to_resolve` exactly.
#' @param max_dist Integer; maximum distance to use during fuzzy matching.
#' @param syn_select List of character vectors. In the case that multiple synonyms
#' were detected during matching, use the names specified. Names in each vector must match names in
#' `scientificName` column of `taxonomic_standard` exactly.
#' @param check_key Logical; should a check performed before joining resolved
#' and unresolved names that all name keys are unique? TRUE by default. If set
#' to FALSE, may cause repeat entries, so use with care!
#'
#' @return Tibble
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' names_df <- tibble::tibble(
#'   sciname = c("Lycopodium kinabaluense Ching", "Gonocormus minutus (Blume) Bosch"),
#'   species = c("Lycopodium kinabaluense", "Gonocormus minutus")
#' )
#'
#' resolve_names_full(
#'   names_to_resolve = names_df,
#'   taxonomic_standard = filmy_taxonomy,
#'   match_order = c("sciname", "species"),
#'   max_dist = c(
#'     sciname = 0,
#'     species = 0
#'   )
#' )
#'
#' resolve_names_full(
#'   names_to_resolve = names_df,
#'   taxonomic_standard = filmy_taxonomy,
#'   match_order = c("sciname"),
#'   max_dist = c(
#'     sciname = 0
#'   )
#' )
#'
#' @export
resolve_names_full <- function(names_to_resolve, taxonomic_standard,
                               exclude = NULL,
                               match_order = c("sciname", "taxon", "species"),
                               max_dist = c(
                                 scientific_name = 7,
                                 taxon = 2,
                                 species = 1
                               ),
                               syn_select = NULL,
                               check_key = TRUE) {

  missing_cols <- match_order[!match_order %in% colnames(names_to_resolve)]

  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = glue::glue("The following columns missing from input: {paste(missing_cols, collapse = ', ')}")
  )

  # Add non-DarwinCore columns to taxonomic_standard that
  # are needed for matching.
  taxonomic_standard <- add_non_darwin_core_cols(taxonomic_standard) %>%
    # Make sure taxon ID and acceptedNameUsageID are integer
    dplyr::mutate(taxonID = as.integer(taxonID)) %>%
    dplyr::mutate(acceptedNameUsageID = as.integer(acceptedNameUsageID))

  checkr::check_data(taxonomic_standard, values = list(
    taxonID = 1L,
    acceptedNameUsageID = c(1L, NA),
    taxonomicStatus = "a",
    scientificName = "a",
    genericName = "a",
    specificEpithet = c("a", NA),
    infraspecificEpithet = c("a", NA),
    taxonName = "a"),
    key = "taxonID")

  raw_names <- dplyr::select(names_to_resolve, match_order) %>%
    assertr::verify(
      nrow(unique(.)) == nrow(.)
    ) %>%
    dplyr::mutate(key = 1:nrow(.))

  resolve_names_results <- list()
  unresolved_names <- list()

  # Resolve by scientific name first.
  # See reports/create_exclude-list.R for how to do initial run to determine exclude lists.
  resolve_names_results[[1]] <- resolve_names(
    names_to_resolve = raw_names %>% dplyr::pull(match_order[1]),
    match_by = match_order[1],
    taxonomic_standard = taxonomic_standard,
    max_dist = max_dist[match_order[1]],
    exclude = exclude[[match_order[1]]],
    mult_syn_selection = syn_select[[match_order[1]]]
  ) %>%
    dplyr::left_join(
      dplyr::select(raw_names, query = !!as.name(match_order[1]), key)
    )

  unresolved <-
    dplyr::filter(resolve_names_results[[1]], taxonomicStatus == "unresolved") %>%
    dplyr::pull(query)

  unresolved_names[[1]] <-
    raw_names %>% dplyr::filter(!!as.name(match_order[1]) %in% unresolved)

  if(!is.na(match_order[2])) {

    resolve_names_results[[2]] <- resolve_names(
      names_to_resolve = unresolved_names[[1]] %>% dplyr::pull(match_order[2]),
      match_by = match_order[2],
      taxonomic_standard = taxonomic_standard,
      max_dist = max_dist[match_order[2]],
      exclude = exclude[[match_order[2]]],
      mult_syn_selection = syn_select[[match_order[2]]]
    ) %>%
      dplyr::left_join(
        dplyr::select(raw_names, query = !!as.name(match_order[2]), key)
      )

    unresolved <-
      dplyr::filter(resolve_names_results[[2]], taxonomicStatus == "unresolved") %>%
      dplyr::pull(query)

    unresolved_names[[2]] <-
      raw_names %>% dplyr::filter(!!as.name(match_order[2]) %in% unresolved)

  }

  if(!is.na(match_order[3])) {

    resolve_names_results[[3]] <- resolve_names(
      names_to_resolve = unresolved_names[[2]] %>% dplyr::pull(match_order[3]),
      match_by = match_order[3],
      taxonomic_standard = taxonomic_standard,
      max_dist = max_dist[match_order[3]],
      exclude = exclude[[match_order[3]]],
      mult_syn_selection = syn_select[[match_order[3]]]
    ) %>%
      dplyr::left_join(
        dplyr::select(raw_names, query = !!as.name(match_order[3]), key)
      )

  }

  final_resolved <-
    dplyr::bind_rows (resolve_names_results) %>%
    dplyr::filter(taxonomicStatus != "unresolved") %>%
    unique %>%
    dplyr::left_join(raw_names)

  final_unresolved <-
    raw_names %>%
    dplyr::anti_join(
      final_resolved, by = "key"
    ) %>%
    dplyr::mutate(taxonomicStatus = "unresolved") %>%
    unique

  results <-
    dplyr::bind_rows(
      final_resolved,
      final_unresolved
    )

  if (isTRUE(check_key)) {
    results <- results %>%
      assertr::assert(assertr::is_uniq, key) %>%
      assertr::assert(nrow(.) == nrow(raw_names)) %>%
      dplyr::select(-key)
  }

  return(results)
}
