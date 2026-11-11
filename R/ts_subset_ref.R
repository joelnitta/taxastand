#' Keep one or more taxa (and their synonyms) from a taxonomic table
#'
#' Subsets a large table of taxonomic names to the members of one or more taxa
#' such as a family, so that a reference small enough for the task at hand can
#' be made. `x` should be in Darwin Core format, or close to it (use `col_map`
#' for columns with other names). Run `ts_subset_ref()` on the
#' table before \code{\link{ts_format_ref}()}, which removes the columns that
#' `ts_subset_ref()` uses to find the members.
#'
#' How members are found depends on the columns in `x`:
#'
#' 1. If `x` has columns for higher ranks (`kingdom`, `phylum`, `class`,
#'    `order`, `family`, `genus` and a few others), rows are kept when one of
#'    these columns is equal to `taxon`.
#' 2. Otherwise, if `x` has a `parentNameUsageID` column, the accepted name
#'    `taxon` is looked up in `scientificName` (the author may follow it) and
#'    all of its descendants are kept.
#' 3. If `x` has neither, there is no hierarchy to use and an error is raised.
#'
#' Next, synonyms of the kept names are added (through
#' `acceptedNameUsageID`), because synonyms often have no hierarchy columns.
#' Last, the accepted names that kept synonyms point to are added, even if they
#' are outside `taxon`, so that every synonym in the result has its accepted
#' name.
#'
#' @param x Dataframe of taxonomic names in Darwin Core format, or close to it,
#'   with at least `taxonID` and `acceptedNameUsageID`, and either columns for
#'   higher ranks or `parentNameUsageID` (and `scientificName`).
#' @param taxon Character vector of taxon names to keep, such as
#'   `"Polypodiaceae"` or `c("Polypodiaceae", "Dryopteridaceae")`. Rows for
#'   all of them are kept.
#' @param rank Optional character; the rank of `taxon`, such as `"family"`.
#'   Use it when the same name is used at more than one rank. By default any
#'   rank matches.
#'
#' @param col_map Named character vector to map columns of `x` to Darwin Core
#'   names, as in \code{\link{ts_format_ref}()}. The same `col_map` can be
#'   given to both functions. Default `NULL`.
#'
#' @return The rows of `x` for the members of `taxon` and their synonyms, with
#'   the same columns as `x`. A warning is given for any name in `taxon` that
#'   was not found; it is an error if none were found.
#' @export
#' @examples
#' x <- data.frame(
#'   taxonID = c("1", "2", "3", "4", "5"),
#'   acceptedNameUsageID = c("1", "2", "3", "1", "5"),
#'   taxonomicStatus = c("accepted", "accepted", "accepted", "synonym", "accepted"),
#'   scientificName = c(
#'     "Polypodium vulgare L.", "Pteris vittata L.", "Pteris cretica L.",
#'     "Polypodium rotundatum Willd.", "Equisetum arvense L."
#'   ),
#'   family = c("Polypodiaceae", "Pteridaceae", "Pteridaceae", NA, "Equisetaceae")
#' )
#' ts_subset_ref(x, "Pteridaceae")
#' ts_subset_ref(x, c("Polypodiaceae", "Equisetaceae"))
ts_subset_ref <- function(x, taxon, rank = NULL, col_map = NULL) {
  assertthat::assert_that(inherits(x, "data.frame"))
  assertthat::assert_that(
    is.character(taxon), length(taxon) >= 1, !anyNA(taxon), all(nzchar(taxon))
  )
  assertthat::assert_that(
    is.null(rank) || (is.character(rank) && length(rank) == 1 && !is.na(rank))
  )
  orig <- x
  x <- apply_col_map(x, col_map)
  need <- c("taxonID", "acceptedNameUsageID")
  missing_cols <- setdiff(need, colnames(x))
  assertthat::assert_that(
    length(missing_cols) == 0,
    msg = paste0(
      "`x` is missing required columns: ", paste(missing_cols, collapse = ", ")
    )
  )

  taxon <- unique(trimws(taxon))
  rank_cols <- intersect(subset_rank_columns(), colnames(x))
  if (!is.null(rank)) rank <- tolower(trimws(rank))
  id <- as.character(x$taxonID)

  found <- rep(FALSE, length(taxon))
  keep <- rep(FALSE, nrow(x))

  if (length(rank_cols) > 0 && (is.null(rank) || rank %in% rank_cols)) {
    # (1) higher-rank columns
    cols <- if (is.null(rank)) rank_cols else rank
    for (col in cols) {
      v <- trimws(as.character(x[[col]]))
      hit <- !is.na(v) & v %in% taxon
      keep <- keep | hit
      found <- found | taxon %in% v
    }
  } else if ("parentNameUsageID" %in% colnames(x) &&
    "scientificName" %in% colnames(x)) {
    # (2) walk down from the named taxon
    assertthat::assert_that(
      is.null(rank) || "taxonRank" %in% colnames(x),
      msg = "`rank` was given but `x` has no `taxonRank` column"
    )
    nm <- trimws(as.character(x$scientificName))
    accepted <- is.na(x$acceptedNameUsageID) |
      as.character(x$acceptedNameUsageID) == id
    start <- rep(FALSE, nrow(x))
    for (i in seq_along(taxon)) {
      hit <- accepted & !is.na(nm) &
        (nm == taxon[i] | startsWith(nm, paste0(taxon[i], " ")))
      if (!is.null(rank)) hit <- hit & tolower(trimws(x$taxonRank)) %in% rank
      hit[is.na(hit)] <- FALSE
      found[i] <- any(hit)
      start <- start | hit
    }
    parent <- as.character(x$parentNameUsageID)
    keep <- start
    repeat {
      add <- !keep & !is.na(parent) & parent %in% id[keep]
      if (!any(add)) break
      keep <- keep | add
    }
  } else {
    stop(
      "Can't find the members of `taxon`: `x` has no columns for higher ranks ",
      "(such as `family`) and no `parentNameUsageID` column, so there is no ",
      "hierarchy to subset with.",
      call. = FALSE
    )
  }

  if (!any(found)) {
    stop("None of `taxon` was found in `x`.", call. = FALSE)
  }
  if (!all(found)) {
    warning(
      "Not found in `x`: ", paste(taxon[!found], collapse = ", "),
      call. = FALSE
    )
  }

  # add synonyms of kept names, then the accepted names of kept synonyms
  acc <- as.character(x$acceptedNameUsageID)
  keep <- keep | (!is.na(acc) & acc %in% id[keep])
  keep <- keep | (id %in% acc[keep])

  # rows of the original table, so the same `col_map` still works afterwards
  orig[keep, , drop = FALSE]
}

# Columns that may hold the higher taxa of a name, used by ts_subset_ref()
subset_rank_columns <- function() {
  c(
    "kingdom", "phylum", "class", "order", "superfamily", "family",
    "subfamily", "tribe", "subtribe", "genus"
  )
}
