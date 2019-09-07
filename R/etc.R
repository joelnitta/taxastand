#' Check data for presence and format of Darwin Core columns
#'
#' @param data Dataframe
#'
#' @return TRUE if passes test, or warning if not
#'
#' @examples
#' \dontrun{
#'
#' data(filmy_taxonomy)
#' check_darwin_core_cols(filmy_taxonomy)
#'
#' badly_formatted_data <- dplyr::select(filmy_taxonomy, -taxonRank)
#' check_darwin_core_cols(badly_formatted_data)
#'
#' }
check_darwin_core_cols <- function (data) {

  # Check for presenc of minimal Darwin Core columns
  dwc_min_cols <- c("taxonID", "acceptedNameUsageID",
                    "taxonomicStatus", "taxonRank",
                    "genus",
                    "scientificName", "specificEpithet", "infraspecificEpithet")

  missing_cols <- setdiff(dwc_min_cols, colnames(data))

  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = glue::glue(
      "The following columns missing from {deparse(substitute(data))}:
      {paste(missing_cols, collapse = ', ')}")
  )

  # Check for format of columns
  assertthat::assert_that(is.numeric(data$taxonID))
  assertthat::assert_that(is.numeric(data$acceptedNameUsageID))
  assertthat::assert_that(is.character(data$taxonomicStatus))
  assertthat::assert_that(is.character(data$taxonRank))
  assertthat::assert_that(is.character(data$genus))
  assertthat::assert_that(is.character(data$scientificName))
  assertthat::assert_that(is.character(data$specificEpithet))
  assertthat::assert_that(is.character(data$infraspecificEpithet))

}

#' Check that data used as taxonomic standard meets Darwin Core format
#'
#' @param taxonomic_standard Dataframe, should be meet Darwin Core format
#'
#' @return Tibble, or error if taxonomic_standard doesn't meet Darwin Core format
#'
#' @examples
#' \dontrun{
#'
#' data(filmy_taxonomy)
#' check_darwin_core_format(filmy_taxonomy)
#'
#' badly_formatted_data <- dplyr::select(filmy_taxonomy, -taxonRank)
#' check_darwin_core_format(badly_formatted_data)
#'
#' }
check_darwin_core_format <- function (taxonomic_standard) {

  check_darwin_core_cols(taxonomic_standard)

  # Early exit if zero rows
  if(nrow(taxonomic_standard) == 0) return(TRUE)

  # If there's data, conduct additional tests
  taxonomic_standard %>%
    assertr::assert(
      assertr::not_na,
      taxonID, taxonomicStatus, taxonRank,
           genus, scientificName, specificEpithet
    ) %>%
    assertr::assert(
      assertr::is_uniq,
      taxonID,
      success_fun = assertr::success_logical
    )

}

#' Add non-DarwinCore columns to taxonomic_standard that
#' are needed for matching.
#'
#' Adds "genericName", "taxonName", and "speciesName".
#'
#' "genus" is always the genus of the accepted
#' name, not the synonym (original name).
#' But for matching we want the genus of the original name
#' Some databases (e.g., CoL) provide this as "genericName".
#' If "genericName" is not already present in the user-provided database,
#' add it by using the first part of the scientificName.
#'
#' "speciesName" is 'genus specific_epithet',
#' e.g. 'Crepidomanes minutum'
#'
#' "taxonName" is 'genus specific_epithet infraspecific_epithet',
#' e.g. 'Crepidomanes minutum flabellatum'
#'
#' @param taxonomic_standard Dataframe of standard names to match to.
#' Must follow [Darwin Core format](https://dwc.tdwg.org/terms/).
#'
#' @return Tibble
#'
#' @examples
#' \dontrun{
#' data(filmy_taxonomy)
#' add_non_darwin_core_cols(filmy_taxonomy)
#' }
add_non_darwin_core_cols <- function (taxonomic_standard) {

  # Genus name (of original taxon, NOT prefered synonym)
  if (!"genericName" %in% colnames(taxonomic_standard)) {
    taxonomic_standard <-
      taxonomic_standard %>%
      dplyr::mutate(
        genericName = stringr::str_split(scientificName, " ") %>% purrr::map_chr(1)
      )
  }

  # Species name (without infrasp. taxon), e.g., "Homo sapiens"
  if (!"speciesName" %in% colnames(taxonomic_standard)) {
    taxonomic_standard <-
      taxonomic_standard %>%
      dplyr::mutate(
        speciesName = jntools::paste3(genericName, specificEpithet)
      )
  }

  # Most specific taxon name (incl. infrasp. taxon if it exists),
  # e.g. "Trichomanes radicans andrewsii"
  if (!"taxonName" %in% colnames(taxonomic_standard)) {
    taxonomic_standard <-
      taxonomic_standard %>%
      dplyr::mutate(
        taxonName = jntools::paste3(genericName, specificEpithet, infraspecificEpithet)
      )
  }

  taxonomic_standard

}

#' Extract only the genus name from a longer name.
#'
#' It is assumed that the first part of the name is the genus.
#' No checking is done for this.
#'
#' @param taxon_name Taxon name, e.g. "Crepidomanes minutum var minutum".
#' @param sep Character separating parts of the name.
#'
#' @return The first two parts of the name separated by space.
#' @examples
#' \dontrun{
#' genus_name_only("Crepidomanes minutum var minutum")
#' }
genus_name_only <- function (taxon_name, sep = " ") {

  assertthat::assert_that(is.character(taxon_name))

  stringr::str_split(taxon_name, sep) %>%
    purrr::map_chr(., ~magrittr::extract(., 1))
}

#' Extract only the species name from a longer name.
#'
#' It is assumed that the first two parts of the name are genus then
#' specific epithet. No checking is done for this.
#'
#' @param taxon_name Taxon name, e.g. "Crepidomanes minutum var minutum".
#' @param sep Character separating parts of the name.
#'
#' @return The first two parts of the name separated by space.
#' @examples
#' sp_name_only("Crepidomanes minutum var minutum")
#' @export
sp_name_only <- function (taxon_name, sep = " ") {

  assertthat::assert_that(is.character(taxon_name))

  stringr::str_split(taxon_name, sep) %>%
    purrr::map_chr(., ~magrittr::extract(., 1:2) %>% jntools::paste3(collapse = sep))
}


#' Convert the first letter of a string to uppercase
#'
#' @param x Character vector
#'
#' @return Character vector with first letter of
#' each element capitalized.
#'
#' @examples
#' \dontrun{
#' toupper_first(c("hi my name", "is"))
#' }
toupper_first <- function (x) {
  assertthat::assert_that(is.character(x))
  paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))
}
