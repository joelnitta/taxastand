#' Format a taxonomic table as a reference for taxastand
#'
#' Turns a table of taxonomic names in (or close to) Darwin Core format, for
#' example from a downloaded taxonomic database, into a reference that can be
#' used by \code{\link{ts_match_names}()}, \code{\link{ts_resolve_names}()} and
#' \code{\link{ts_optimize_ref}()}.
#'
#' `ts_format_ref()` does these steps:
#'
#' 1. Renames columns according to `col_map`.
#' 2. Adds the author to the species name, if not already included. Matching
#'    uses the author, so it must be part of `scientificName`. Many databases
#'    keep the author in a separate column, `scientificNameAuthorship`; if so,
#'    these are combined.
#' 3. Drops rows whose `taxonRank` is above species, such as genus or family
#'    (see `drop_ranks`).
#' 4. Tidies values: `taxonomicStatus` is lower-cased and trimmed, empty
#'    strings become `NA`, and rows with no name and exact duplicate rows are
#'    removed.
#'
#' There is no controlled vocabulary for taxonomic ranks, and databases differ
#' in how many infraspecific ranks they use. Rows are therefore dropped using a
#' list of ranks above species, not kept using a list of ranks to accept: a row
#' with any other rank (including an unknown rank or none) is kept.
#'
#' `ts_format_ref()` does not check that the table is valid Darwin Core
#' taxonomic data (for example, that every `acceptedNameUsageID` points to an
#' existing `taxonID`). The
#' [dwctaxon](https://docs.ropensci.org/dwctaxon/) package can validate and
#' edit such tables (see `dwctaxon::dct_validate()`).
#'
#' @param x Dataframe of taxonomic names. Needs columns for `taxonID`,
#'   `acceptedNameUsageID`, `taxonomicStatus` and `scientificName` (see
#'   \code{\link{ts_resolve_names}()} for what they mean), either with those
#'   names or mapped to them with `col_map`. May also have
#'   `scientificNameAuthorship` and `taxonRank`.
#' @param col_map Named character vector to rename columns of `x`. Each name
#'   is the Darwin Core name and each value is the column of `x` that holds it.
#'   For example,
#'   `c(taxonID = "taxon_id", scientificName = "canonical_name")`.
#'   Columns that already have Darwin Core names need not be listed. Default
#'   `NULL`.
#' @param drop_ranks Character vector; rows with a `taxonRank` in this list
#'   (ignoring case) are dropped. The default is the ranks above species rank
#'   (kingdom to genus, and the ranks between them such as subfamily, tribe,
#'   section and series). Use `NULL` to keep all rows. Ignored if `x`
#'   has no `taxonRank` column.
#' @param source Optional list describing where the data came from, for
#'   example `list(provider = "ITIS", version = "2026", license = "CC0")`.
#'   It is stored in the `ts_source` attribute of the result.
#'
#' @return A dataframe with columns `taxonID`, `acceptedNameUsageID`,
#'   `taxonomicStatus` and `scientificName` (and `taxonRank`, if `x` had it),
#'   with the `ts_source` attribute if `source` was given.
#' @export
#' @examples
#' # A table with its own column names, and the author in a separate column
#' x <- data.frame(
#'   id = c("1", "2", "3"),
#'   accepted_id = c("1", "1", "3"),
#'   status = c("Accepted", "Synonym", "Accepted"),
#'   name = c("Polypodium vulgare", "Polypodium rotundatum", "Polypodium"),
#'   author = c("L.", "Willd.", "L."),
#'   rank = c("species", "species", "genus")
#' )
#' ts_format_ref(
#'   x,
#'   col_map = c(
#'     taxonID = "id", acceptedNameUsageID = "accepted_id",
#'     taxonomicStatus = "status", scientificName = "name",
#'     scientificNameAuthorship = "author", taxonRank = "rank"
#'   ),
#'   source = list(provider = "Example", version = "1")
#' )
ts_format_ref <- function(x, col_map = NULL, drop_ranks = ts_ranks_above_species(),
                          source = NULL) {
  assertthat::assert_that(inherits(x, "data.frame"))
  assertthat::assert_that(is.null(drop_ranks) || is.character(drop_ranks))
  assertthat::assert_that(is.null(source) || is.list(source))

  x <- as.data.frame(x, stringsAsFactors = FALSE)

  x <- apply_col_map(x, col_map)

  required <- c(
    "taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"
  )
  missing_req <- setdiff(required, colnames(x))
  assertthat::assert_that(
    length(missing_req) == 0,
    msg = paste0(
      "`x` is missing required columns: ",
      paste(missing_req, collapse = ", "),
      ". Use `col_map` to name the columns that hold them."
    )
  )

  # empty strings are missing values
  chr_cols <- intersect(
    c(required, "scientificNameAuthorship", "taxonRank"), colnames(x)
  )
  for (col in chr_cols) {
    v <- trimws(as.character(x[[col]]))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    x[[col]] <- v
  }

  x <- x[!is.na(x$scientificName), , drop = FALSE]

  if ("scientificNameAuthorship" %in% colnames(x)) {
    auth <- x$scientificNameAuthorship
    add <- !is.na(auth) & !endsWith(x$scientificName, auth)
    add[is.na(add)] <- FALSE
    x$scientificName[add] <- paste(x$scientificName[add], auth[add])
  }

  x$taxonomicStatus <- tolower(x$taxonomicStatus)

  if ("taxonRank" %in% colnames(x)) {
    x$taxonRank <- tolower(x$taxonRank)
    x <- x[is.na(x$taxonRank) | !x$taxonRank %in% tolower(drop_ranks), ,
      drop = FALSE]
  }

  out <- x[, intersect(c(required, "taxonRank"), colnames(x)), drop = FALSE]
  out <- out[!duplicated(out), , drop = FALSE]
  rownames(out) <- NULL
  if (!is.null(source)) attr(out, "ts_source") <- source
  out
}

#' Ranks above species
#'
#' Default for the `drop_ranks` argument of \code{\link{ts_format_ref}()}.
#'
#' @return Character vector of rank names, in lower case.
#' @keywords internal
ts_ranks_above_species <- function() {
  c(
    "domain", "superkingdom", "kingdom", "subkingdom", "infrakingdom",
    "superphylum", "phylum", "division", "subphylum", "subdivision",
    "infraphylum", "superclass", "class", "subclass", "infraclass",
    "subterclass", "parvclass", "superorder", "order", "suborder", "infraorder",
    "parvorder", "superfamily", "epifamily", "family", "subfamily",
    "infrafamily", "supertribe", "tribe", "subtribe", "infratribe", "genus",
    "subgenus", "infragenus", "section", "subsection", "series", "subseries",
    "superspecies", "species group", "species subgroup"
  )
}

# Rename the columns of `x` to Darwin Core names using `col_map` (see
# ts_format_ref()). Used by ts_format_ref() and ts_subset_ref().
apply_col_map <- function(x, col_map) {
  assertthat::assert_that(is.null(col_map) || (
    is.character(col_map) && !is.null(names(col_map)) &&
      all(nzchar(names(col_map))) && !anyNA(col_map)
  ), msg = "`col_map` must be a named character vector")
  if (is.null(col_map)) {
    return(x)
  }
  missing_cols <- setdiff(col_map, colnames(x))
  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = paste0(
      "`col_map` refers to columns not in `x`: ",
      paste(missing_cols, collapse = ", ")
    )
  )
  # the mapped column replaces any column that already has the Darwin Core name
  renamed <- x[, unname(col_map), drop = FALSE]
  colnames(renamed) <- names(col_map)
  cbind(x[, setdiff(colnames(x), names(col_map)), drop = FALSE], renamed)
}
