#' Match species names to a reference database
#'
#' Match a vector of species names to a user-provided taxonomic reference
#' database in the [Darwin Core format](https://dwc.tdwg.org/terms/).
#'
#' For an example of a taxonomic reference database in Darwin Core format,
#' see \code{\link{filmy_taxonomy}}.
#'
#' Fuzzy matching can be used by setting `max_dist` > 0. Fuzzy matching is
#' particularly useful for full scientific names (including author), as author
#' abbreviations and formatting vary widely.
#'
#' It is assumed that all names in `query` are of the same type. Types of
#' species names include:
#' - species: Species name not including any names below the
#' species level, e.g., "Iris pallida"
#' - taxon: Species name including names below the species level (if any),
#' e.g., "Iris pallida cengialti". Note that the rank of the infraspecific
#' name (e.g., "var.") is not included.
#' - scientific name: The full species name including infraspecific name (if any)
#' and author, e.g., "Iris pallida subsp. cengialti (Ambrosi ex A.Kern.) Foster".
#'
#' @param query Character vector; taxonomic names to resolve.
#' @param taxonomic_standard Dataframe of standard names to match to.
#' Must follow [Darwin Core format](https://dwc.tdwg.org/terms/).
#' @param match_by One of "species", "taxon", or "sciname" (scientific name);
#' the type of standard name to use for matching.
#' Should match the type of names in `query`.
#' @param max_dist Integer; maximum distance to use during fuzzy matching.
#' @param simple Logical; should the results include only a limited set of
#' columns? If FALSE, all columns in `taxonomic_standard` will be returned.
#'
#' @return Dataframe. Includes the following columns:
#' - `query`: the search query
#' - `n_hits`: the number of matches to the query
#' - `distance`: the number of characters differing between the query
#' and the match
#' - `match_to`: the name in `taxonomic_standard` that matched the query
#' - `match_by`: the type of species name that was used for matching
#'
#' ... and either all columns in `taxonomic_standard` or only a subset of
#' these if `simple` is TRUE.
#'
#' @examples
#' # Load reference taxonomy in Darwin Core format
#' data(filmy_taxonomy)
#'
#' # This taxon matches many names at the species level because
#' # there are a bunch of varieties.
#' match_taxonomy("Hymenophyllum polyanthos", filmy_taxonomy, "species")
#'
#' # Using the full species name with author can get us a
#' # more exact match.
#' match_taxonomy("Hymenophyllum polyanthos (Sw.) Sw.", filmy_taxonomy, "sciname")
#'
#' # Fuzzy match helps when the query didn't abbreviate
#' # the author, but it is abbreviated in the reference.
#' match_taxonomy("Hymenophyllum polyanthos (Swartz) Swartz",
#' filmy_taxonomy, "sciname", max_dist = 8)
#'
#' @export
match_taxonomy <- function (query,
                            taxonomic_standard,
                            match_by = c("species", "taxon", "sciname"),
                            max_dist = 0, simple = TRUE) {

  ### Check input ###
  assertthat::assert_that(is.character(query))
  assertthat::assert_that(is.data.frame(taxonomic_standard))
  assertthat::assert_that(assertthat::is.string(match_by))
  assertthat::assert_that(
    match_by %in% c("species", "taxon", "sciname"),
    msg = "'match_by' must be one of 'species', 'taxon', or 'sciname'")
  assertthat::assert_that(assertthat::is.number(max_dist))
  assertthat::assert_that(is.logical(simple))

  # (Need to add check on taxonomic_standard to ensure it meets DarwinCore)

  ### Setup ###
  # Prepare sets of column names to include in the output.
  # - Column names relevant to the match algorithm
  match_cols <- c(
    "query",    # input query
    "n_hits",   # number of hits to that query
    "distance", # number of characters different between query and match
    "match_to", # name that matched the query
    "match_by") # what type of matching was used (species, taxon, or sci. name)

  # - Selected DarwinCore taxonomy columns to include
  # if results are "simple"
  darwin_simple_cols <- c(
    "taxonID", "acceptedNameUsageID",
    "taxonomicStatus", "taxonRank",
    "scientificName", "genus", "specificEpithet", "infraspecificEpithet"
  )

  # - All colnames in original taxonomic standard dataframe
  # (should all be DarwinCore columns).
  darwin_original_cols <- colnames(taxonomic_standard)

  # Convert input name vector to tibble.
  names_to_resolve <- tibble::tibble(query = query)

  # Set name to use for matching to taxonomic standard.
  col_standard <- switch(match_by,
                         species = "speciesName",
                         taxon = "taxonName",
                         sciname = "scientificName")

  # Add non-DarwinCore columns to taxonomic_standard that
  # are needed for matching.

  # One gray area is "genericName". "genus" is always the genus of the accepted
  # name, not the synonym. But for matching we want the genus of the synonym.
  # Some databases (e.g., CoL) provide this as "genericName".
  # If "genericName" is not already present in the user-provided database,
  # add it by using the first part of the scientificName.

  if (!"genericName" %in% colnames(taxonomic_standard)) {
    taxonomic_standard <-
      taxonomic_standard %>%
      dplyr::mutate(
      genericName = stringr::str_split(scientificName, " ") %>% purrr::map_chr(1)
    )
  }

  taxonomic_standard <-
    taxonomic_standard %>%
    dplyr::mutate(
      # Species name (without infrasp. taxon), e.g., "Homo sapiens"
      speciesName = jntools::paste3(genericName, specificEpithet),
      # Most specific taxon name (incl. infrasp. taxon if it exists),
      # e.g. "Trichomanes radicans andrewsii"
      taxonName = jntools::paste3(genericName, specificEpithet, infraspecificEpithet)
    )

  ### Find matches ###
  # Use fuzzy matching if max_dist > 0
  if (max_dist > 0) {

    hits <- fuzzyjoin::stringdist_inner_join(
      x = names_to_resolve,
      # fuzzyjoin can't handle NAs in match column
      y = taxonomic_standard %>% dplyr::filter(!is.na(!!rlang::sym(col_standard))),
      by = c(query = col_standard),
      max_dist = max_dist,
      distance_col = "distance"
    ) %>%
      # Add the number of matches per query
      dplyr::group_by(query) %>%
      dplyr::mutate(n_hits = length(query)) %>%
      dplyr::ungroup() %>%
      # Add name the query matched to
      dplyr::mutate(match_to = !!rlang::sym(col_standard)) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

    # Use exact matching otherwise
  } else {

    hits <- dplyr::inner_join(
      x = names_to_resolve,
      y = taxonomic_standard,
      by = c(query = col_standard)) %>%
      dplyr::group_by(query) %>%
      dplyr::mutate(n_hits = length(query)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        distance = 0,
        # Add name the query matched to
        match_to = query,
        # Add match method
        match_by = match_by,
        # Note inner_join drops the RHS column after matching,
        # so add this back in
        !!rlang::sym(col_standard) := query)
  }

  # Find non-matches
  # Use fuzzy matching if max_dist > 0
  if (max_dist > 0) {

    missing <- fuzzyjoin::stringdist_anti_join(
      x = names_to_resolve,
      y = taxonomic_standard %>% dplyr::filter(!is.na(!!rlang::sym(col_standard))),
      by = c(query = col_standard),
      max_dist = max_dist,
      distance_col = "distance"
    ) %>%
      dplyr::mutate(n_hits = 0) %>%
      # Add name the query matched to (NA for non-hits)
      dplyr::mutate(match_to = NA_character_) %>%
      # Add distance to match (NA for non-hits)
      dplyr::mutate(distance = NaN) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

    # Use exact matching otherwise
  } else {

    missing <- dplyr::anti_join(
      x = names_to_resolve,
      y = taxonomic_standard,
      by = c(query = col_standard)
    ) %>%
      dplyr::mutate(n_hits = 0) %>%
      # Add name the query matched to (NA for non-hits)
      dplyr::mutate(match_to = NA_character_) %>%
      # Add distance to match (NA for non-hits)
      dplyr::mutate(distance = NaN) %>%
      # Add match method
      dplyr::mutate(match_by = match_by)

  }

  # Make sure we didn't miss anything
  assertthat::assert_that(
    all(c(hits$query, missing$query) %in% names_to_resolve$query),
    msg = "One or more queried names missing from results"
  )

  ### Combine and output results ###
  results <- dplyr::bind_rows(hits, missing) %>%
    dplyr::select(c(match_cols, darwin_original_cols))

  if(isTRUE(simple)) results <- dplyr::select(
    results, c(match_cols,  darwin_simple_cols)
  )

  results

}
