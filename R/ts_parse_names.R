#' Parse taxonomic names
#'
#' Parses scientific names into their component parts (genus, species, variety,
#' author, etc).
#'
#' @param taxa Character vector; taxon names to be parsed. Missing values not
#' allowed. Must all be unique.
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#' @param quiet Logical; if TRUE, suppress warning messages that would normally
#' be issued
#' @param strip_qualifiers Logical; if `TRUE`, remove standalone
#' open-nomenclature qualifiers (`cf.` and `aff.`, as reviewed by Sigovini,
#' Keppel, and Tagliapietra (2016); see References) from names before
#' parsing, so e.g. `"Tectaria cf. crenata"` parses the same as `"Tectaria
#' crenata"` instead of mis-capturing the qualifier as the specific epithet.
#' Also strips `s.l.`/`sensu lato` and `s.str.`/`sensu stricto`, an older,
#' separate circumscription convention not covered by that review but
#' included here as a taxastand extension. Default: `FALSE`, to preserve
#' prior behavior.
#'
#' @return A dataframe including the following columns.
#' - id: A unique ID number assigned to the input name
#' - name: The input name
#' - genus_hybrid_sign: Hybrid sign for genus
#' - genus_name: Genus name
#' - species_hybrid_sign: Hybrid sign for species
#' - specific_epithet: Specific epithet (name)
#' - infraspecific_rank: Infraspecific rank
#' - infraspecific_epithet: Infraspecific epithet (name)
#' - author: Name of taxon
#'
#' @references Sigovini, M., Keppel, E., & Tagliapietra, D. (2016). Open
#' Nomenclature in the biodiversity era. Methods in Ecology and Evolution,
#' 7(10), 1217-1225. \doi{10.1111/2041-210X.12594}
#'
#' @autoglobal
#' @export
#' @examples
#' ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#' ts_parse_names(
#'   "Foogenus x barspecies var. foosubsp (L.) F. Bar",
#'   tbl_out = TRUE
#' )
#'
#' # If you always want tibble output without specifying `tbl_out = TRUE`
#' # every time, set the option:
#' options(ts_tbl_out = TRUE)
#' ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#' ts_parse_names("Crepidomanes minutum (Blume) K. Iwats.")
#'
#' # Strip an open-nomenclature qualifier before parsing
#' ts_parse_names("Tectaria cf. crenata", strip_qualifiers = TRUE)
#'
ts_parse_names <- function(
  taxa,
  tbl_out = getOption("ts_tbl_out", default = FALSE),
  quiet = FALSE,
  strip_qualifiers = FALSE
) {
  # Check input: must be character vector, no NA values, all unique
  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(
    assertthat::noNA(taxa),
    msg = "Input taxa may not contain NAs"
  )
  assertthat::assert_that(
    !anyDuplicated(taxa),
    msg = "Input taxa must be unique"
  )
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))

  # Format input names for parsing as `id|taxon_name` records, for example
  # `x-234|Foogenus x barspecies var. foosubsp (L.) F. Bar` # nolint: commented_code_linter.
  taxa_tbl <- ts_make_name_df(taxa)
  taxa_tbl$record <- paste(taxa_tbl$id, taxa_tbl$name, sep = "|")

  # Parse names 
  parsed_lines <- tt_parsenames(
    taxa_tbl$record,
    strip_qualifiers = strip_qualifiers
  )

  # Read in results of parsing, format as dataframe

  # The output is one record per line, with fields separated by '|' (pipe symbol)
  parsed_names <- data.frame(record = parsed_lines)

  # Split these into separate columns
  name_parts <- c(
    "genus_hybrid_sign",
    "genus_name",
    "species_hybrid_sign",
    "specific_epithet",
    "infraspecific_rank",
    "infraspecific_epithet",
    "author"
  )

  parsed_names <- tidyr::separate(
    data = parsed_names,
    col = record,
    into = c("id", name_parts),
    sep = "\\|",
    fill = "right",
    remove = FALSE
  )

  # Fill in NA if that name part is missing
  parsed_names[parsed_names == ""] <- NA

  # Add "fail" column if all name parts are missing (couldn't be parsed properly)
  parsed_names$fail <- vapply(
    seq_len(nrow(parsed_names)),
    function(x) all(is.na(parsed_names[x, name_parts])),
    logical(1)
  )

  # Early exit if everything failed
  assertthat::assert_that(
    !all(parsed_names$fail),
    msg = "No names could be successfully parsed"
  )

  # Emit warning for failures
  if (sum(parsed_names$fail) > 0 && !quiet) {
    failed_ids <- parsed_names$id[parsed_names$fail]
    failed_names <- paste(
      taxa_tbl$name[taxa_tbl$id %in% failed_ids],
      collapse = ", "
    )
    warning(glue::glue(
      "The following names could not be parsed and are excluded from results: {failed_names}"
    ))
  }

  # Add back in original name
  parsed_names <- dplyr::left_join(
    parsed_names,
    dplyr::select(taxa_tbl, id, name),
    by = "id"
  )

  # Remove failures, drop "fail" column
  parsed_names <- parsed_names[!parsed_names$fail, ]
  parsed_names$fail <- NULL

  # Return parsed names as dataframe or tibble
  results <- parsed_names[, c("name", "id", name_parts)]

  if (isTRUE(tbl_out)) {
    return(tibble::as_tibble(results))
  }

  results
}
