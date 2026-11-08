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
#' @param exclude_placeholders Logical; if `TRUE` (default), names that are
#' placeholders for an identification that stopped above species rank (`sp.`,
#' `spp.`, `indet.`, `stet.`, `inc.`, and their long forms; see References) are
#' excluded from the results with a warning, like names that cannot be parsed.
#' Otherwise `"Genus sp."` would be parsed as if `"sp"` were a real specific
#' epithet and could then be fuzzy-matched to unrelated species.
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
  strip_qualifiers = FALSE,
  exclude_placeholders = TRUE
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
  assertthat::assert_that(assertthat::is.flag(exclude_placeholders))

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
  name_parts <- c(
    "genus_hybrid_sign",
    "genus_name",
    "species_hybrid_sign",
    "specific_epithet",
    "infraspecific_rank",
    "infraspecific_epithet",
    "author"
  )

  # Split into the id and the name parts (8 fields; fields may be empty). Fill
  # in NA if that name part is missing.
  fields <- .tt_split_fields(parsed_lines, length(name_parts) + 1L)
  parsed_names <- data.frame(id = fields[[1]])
  for (i in seq_along(name_parts)) {
    field <- fields[[i + 1L]]
    field[field == ""] <- NA_character_
    parsed_names[[name_parts[i]]] <- field
  }

  # Add "fail" column if all name parts are missing (couldn't be parsed properly)
  parsed_names$fail <- rowSums(!is.na(parsed_names[, name_parts])) == 0

  # Placeholder names (e.g. "Genus sp.") are not species names: exclude them
  placeholder <- rep(FALSE, nrow(parsed_names))
  if (exclude_placeholders) {
    orig_name <- taxa_tbl$name[match(parsed_names$id, taxa_tbl$id)]
    placeholder <- .ts_is_placeholder(orig_name) & !parsed_names$fail
    if (any(placeholder) && !quiet) {
      warning(
        "The following names are placeholders for an identification above ",
        "species rank and are excluded from results: ",
        paste(orig_name[placeholder], collapse = ", "),
        call. = FALSE
      )
    }
    parsed_names$fail <- parsed_names$fail | placeholder
  }

  # Early exit if everything failed
  assertthat::assert_that(
    !all(parsed_names$fail),
    msg = "No names could be successfully parsed"
  )

  # Emit warning for failures
  unparsed <- parsed_names$fail & !placeholder
  if (any(unparsed) && !quiet) {
    failed_ids <- parsed_names$id[unparsed]
    failed_names <- paste(
      taxa_tbl$name[taxa_tbl$id %in% failed_ids],
      collapse = ", "
    )
    warning(
      "The following names could not be parsed and are excluded from results: ",
      failed_names,
      call. = FALSE
    )
  }

  # Add back in original name
  parsed_names$name <- taxa_tbl$name[match(parsed_names$id, taxa_tbl$id)]

  # Remove failures, drop "fail" column
  parsed_names <- parsed_names[!parsed_names$fail, ]
  parsed_names$fail <- NULL

  # Return parsed names as dataframe or tibble
  results <- parsed_names[, c("name", "id", name_parts)]

  if (isTRUE(tbl_out)) {
    return(.ts_as_tibble(results))
  }

  results
}

# Tokens marking an identification that stopped above species rank (open
# nomenclature; Sigovini et al. 2016): sp./spp., inc./incerta, indet./
# indeterminabilis, stet./stetit. Whole whitespace-delimited tokens only.
.ts_placeholder_re <- paste0(
  "(?<=\\s)(",
  "spp?\\.?|inc\\.?|incerta|indet\\.?|indeterminabilis|stet\\.?|stetit",
  ")(?=\\s|$)"
)

.ts_is_placeholder <- function(x) {
  grepl(.ts_placeholder_re, x, perl = TRUE)
}
