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
#' 4. Keeps the required columns, `taxonRank` and the columns that give the
#'    hierarchy (higher ranks such as `family`, and `parentNameUsageID`), and
#'    drops the rest. This lets \code{\link{ts_subset_ref}()} be used after
#'    `ts_format_ref()` for most tables (see its documentation for the
#'    exception).
#' 5. Tidies values: `taxonomicStatus` is lower-cased and trimmed, empty
#'    strings become `NA`, and rows with no name and exact duplicate rows are
#'    removed. A missing `taxonID` (some databases give none to synonyms) is
#'    replaced by a made-up unique one, such as `"taxastand:17"`, because
#'    \code{\link{ts_resolve_names}()} needs every name to have an ID.
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
#' @param ref Dataframe of taxonomic names. Needs columns for `taxonID`,
#'   `acceptedNameUsageID`, `taxonomicStatus` and `scientificName` (see
#'   \code{\link{ts_resolve_names}()} for what they mean), either with those
#'   names or mapped to them with `col_map`. May also have
#'   `scientificNameAuthorship` and `taxonRank`.
#' @param col_map Named character vector to rename columns of `ref`. Each name
#'   is the Darwin Core name and each value is the column of `ref` that holds
#'   it.
#'   For example,
#'   `c(taxonID = "taxon_id", scientificName = "canonical_name")`.
#'   Columns that already have Darwin Core names need not be listed. Default
#'   `NULL`.
#' @param drop_ranks Character vector; rows with a `taxonRank` in this list
#'   (ignoring case) are dropped. The default is the ranks above species rank
#'   (kingdom to genus, and the ranks between them such as subfamily, tribe,
#'   section and series). Use `NULL` to keep all rows. Ignored if `ref`
#'   has no `taxonRank` column.
#' @param source Optional list describing where the data came from, for
#'   example `list(provider = "ITIS", version = "2026", license = "CC0")`.
#'   It is stored in the `ts_source` attribute of the result.
#'
#' @return A dataframe with columns `taxonID`, `acceptedNameUsageID`,
#'   `taxonomicStatus` and `scientificName` (and `taxonRank`, if `ref` had it),
#'   plus any columns for the hierarchy of the names that `ref` had: higher
#'   ranks such as `family` and `parentNameUsageID`. Other columns are dropped.
#'   It
#'   has the `ts_source` attribute if `source` was given.
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
ts_format_ref <- function(
  ref, col_map = NULL,
  drop_ranks = ts_ranks_above_species(),
  source = NULL
) {
  assertthat::assert_that(
    inherits(ref, "data.frame"),
    is.null(drop_ranks) || is.character(drop_ranks),
    is.null(source) || is.list(source)
  )

  ref <- as.data.frame(ref, stringsAsFactors = FALSE)

  ref <- apply_col_map(ref, col_map)

  required <- c(
    "taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"
  )
  missing_req <- setdiff(required, colnames(ref))
  assertthat::assert_that(
    length(missing_req) == 0,
    msg = paste0(
      "`ref` is missing required columns: ",
      toString(missing_req),
      ". Use `col_map` to name the columns that hold them."
    )
  )

  ref <- .ts_ref_tidy_text(ref, required)
  ref$taxonomicStatus <- tolower(ref$taxonomicStatus)
  ref <- .ts_ref_drop_ranks(ref, drop_ranks)

  keep_cols <- c(required, "taxonRank", hierarchy_columns())
  out <- ref[, intersect(keep_cols, colnames(ref)), drop = FALSE]
  out <- out[!duplicated(out), , drop = FALSE]
  # Some databases give no ID to synonyms. Resolving needs every name to have
  # one, so make up unique IDs for these.
  no_id <- is.na(out$taxonID)
  if (any(no_id)) {
    out$taxonID[no_id] <- paste0("taxastand:", seq_len(nrow(out))[no_id])
  }
  rownames(out) <- NULL
  if (!is.null(source)) attr(out, "ts_source") <- source
  out
}

# Empty strings are missing values; drop the rows with no name, and put the
# author after the name unless the name has it already
.ts_ref_tidy_text <- function(ref, required) {
  chr_cols <- intersect(
    c(
      required, "scientificNameAuthorship", "taxonRank",
      hierarchy_columns()
    ),
    colnames(ref)
  )
  for (col in chr_cols) {
    v <- trimws(as.character(ref[[col]]))
    v[!is.na(v) & !nzchar(v)] <- NA_character_
    ref[[col]] <- v
  }

  ref <- ref[!is.na(ref$scientificName), , drop = FALSE]

  if ("scientificNameAuthorship" %in% colnames(ref)) {
    auth <- ref$scientificNameAuthorship
    add <- !is.na(auth) & !endsWith(ref$scientificName, auth)
    add[is.na(add)] <- FALSE
    ref$scientificName[add] <- paste(ref$scientificName[add], auth[add])
  }
  ref
}

# Lower-case the ranks and remove the rows with the ranks in `drop_ranks`
.ts_ref_drop_ranks <- function(ref, drop_ranks) {
  if ("taxonRank" %in% colnames(ref)) {
    ref$taxonRank <- tolower(ref$taxonRank)
    ref <- ref[is.na(ref$taxonRank) | !ref$taxonRank %in% tolower(drop_ranks), ,
      drop = FALSE
    ]
  }
  ref
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

# Rename the columns of `ref` to Darwin Core names using `col_map` (see
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
      "`col_map` refers to columns not in `ref`: ",
      toString(missing_cols)
    )
  )
  # the mapped column replaces any column that already has the Darwin Core name
  renamed <- x[, unname(col_map), drop = FALSE]
  colnames(renamed) <- names(col_map)
  cbind(x[, setdiff(colnames(x), names(col_map)), drop = FALSE], renamed)
}

# Columns that give the hierarchy of a name; kept by the formatting function
# and used by the subsetting function
hierarchy_columns <- function() {
  c(subset_rank_columns(), "parentNameUsageID")
}
