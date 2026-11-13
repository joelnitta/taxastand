#' Prepare a reference for repeated matching
#'
#' Matching names against a reference spends most of its time preparing the
#' reference: finding the genus of every name, parsing the names and building
#' the keys used for comparison. `ts_optimize_ref()` does the first of
#' these once, and the other two lazily, one genus at a time, the first time a
#' query needs that genus. Pass the result to \code{\link{ts_match_names}()}
#' or \code{\link{ts_resolve_names}()} in place of the reference; repeated
#' calls (for example many batches of queries, or an interactive session
#' against a large reference such as WFO or GBIF) then skip work that has
#' already been done.
#'
#' Results are identical to those obtained by passing the reference directly.
#' The object holds the parsed names of every genus used so far, so it grows
#' with use, up to the size of the whole parsed reference.
#'
#' @param ref Character vector of reference names (with authors), or a
#'   dataframe of taxonomic data in Darwin Core format (see
#'   \code{\link{ts_resolve_names}()}). A dataframe can be used for both
#'   \code{\link{ts_match_names}()} and \code{\link{ts_resolve_names}()} (as
#'   `ref`); a character vector only for the former.
#' @param strip_qualifiers Logical; remove standalone open-nomenclature
#'   qualifiers (`cf.`, `aff.`, ...) from the reference names before parsing.
#'   See \code{\link{ts_parse_names}()}. Default: `FALSE`. The setting used
#'   here applies to the reference; the `strip_qualifiers` argument of the
#'   matching functions then applies to the query only.
#' @return A `ts_reference` object.
#' @export
#' @examples
#' data(filmy_taxonomy)
#' ref <- ts_optimize_ref(filmy_taxonomy)
#' ref
#' # Reuse for several calls
#' ts_match_names("Gonocormus minutum", ref, simple = TRUE)
#' ts_resolve_names("Gonocormus minutum", ref)
ts_optimize_ref <- function(ref, strip_qualifiers = FALSE) {
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))
  taxonomy <- NULL
  if (inherits(ref, "data.frame")) {
    assertthat::assert_that(
      "scientificName" %in% colnames(ref),
      msg = "ref must have a `scientificName` column"
    )
    cols <- intersect(
      c("taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"),
      colnames(ref)
    )
    taxonomy <- as.data.frame(ref)[, cols, drop = FALSE]
    ref <- unique(as.character(ref$scientificName))
  }
  assertthat::assert_that(
    is.character(ref),
    msg = "ref must be a character vector or data.frame"
  )
  assertthat::assert_that(
    !anyNA(ref),
    msg = "ref must not contain missing values"
  )
  .ts_new_reference(ref, strip_qualifiers, taxonomy)
}

#' @export
print.ts_reference <- function(x, ...) {
  cat(
    "<ts_reference>", length(x$names), "names in", length(x$genera),
    "genera\n"
  )
  cat("  genera parsed so far:", length(x$state$loaded), "\n")
  invisible(x)
}

# ---- Internals ------------------------------------------------------------
#
# A prepared reference keeps its raw names and the genus key of each, plus a
# mutable `state` environment holding a pool of every record parsed so far.
# The pool is append-only: genera that no query has needed yet are parsed in one
# batch and appended, so a call only has to select the rows of the genera it
# uses (by index) and never has to combine per-genus pieces.
#   loaded                genus keys already in the pool
#   row                   position of each pool record in the reference
#   genus_key             genus key of each pool record
#   code, genus, fields,  as returned by .tt_record_keys(), concatenated over
#   no_auth, auct         the batches
#   batches               one .tt_record_keys() object per batch, whose lazy
#                         key(method) fills `keys` (the pool-wide key vectors,
#                         extended batch by batch as they are asked for)
.ts_new_reference <- function(
  names, strip_qualifiers = FALSE,
  taxonomy = NULL
) {
  genus_key <- .ts_raw_genus_key(names)
  empty <- .tt_record_keys(character())
  state <- new.env(parent = emptyenv())
  state$loaded <- character()
  state$row <- integer()
  state$genus_key <- character()
  state$code <- empty$code
  state$genus <- empty$genus
  state$fields <- empty$fields
  state$no_auth <- empty$no_auth
  state$auct <- empty$auct
  state$batches <- list()
  state$keys <- list()
  state$keys_upto <- list()
  structure(
    list(
      names = names,
      genus_key = genus_key,
      genera = unique(genus_key),
      strip_qualifiers = strip_qualifiers,
      taxonomy = taxonomy,
      state = state
    ),
    class = "ts_reference"
  )
}

# Make sure the genera `keys` are in the pool. All genera that are not there yet
# are parsed together and appended as one batch.
.ts_ref_fill <- function(ref, keys) {
  st <- ref$state
  cold <- setdiff(keys, st$loaded)
  if (!length(cold)) {
    return(invisible())
  }
  rows <- which(ref$genus_key %in% cold)
  parsed <- tryCatch(
    ts_parse_names(
      ref$names[rows],
      strip_qualifiers = ref$strip_qualifiers,
      exclude_placeholders = FALSE
    ),
    error = function(e) {
      # Only error when none of the names could be parsed
      if (!grepl("No names could", conditionMessage(e), fixed = TRUE)) {
        stop(e)
      }
      .ts_empty_parsed_df()
    }
  )
  # Names that could not be parsed are dropped (with a warning); map the rest
  # back to their position in the reference. The ID encodes that position.
  rows <- rows[match(parsed$name, ref$names[rows])]
  parsed$id <- sprintf("ref-%d", rows)
  batch <- .tt_record_keys(.ts_parsed_lines(parsed, rows))
  st$loaded <- c(st$loaded, cold)
  st$row <- c(st$row, rows)
  st$genus_key <- c(st$genus_key, ref$genus_key[rows])
  st$code <- c(st$code, batch$code)
  st$genus <- c(st$genus, batch$genus)
  st$fields <- Map(c, st$fields, batch$fields)
  st$no_auth <- c(st$no_auth, batch$no_auth)
  st$auct <- c(st$auct, batch$auct)
  st$batches[[length(st$batches) + 1L]] <- batch
  invisible()
}

# Comparison key `method` for every record in the pool, built for the batches
# added since it was last asked for
.ts_pool_key <- function(st, method) {
  done <- if (is.null(st$keys_upto[[method]])) 0L else st$keys_upto[[method]]
  n <- length(st$batches)
  if (done < n) {
    new <- unlist(
      lapply(st$batches[(done + 1L):n], function(b) b$key(method)),
      use.names = FALSE
    )
    st$keys[[method]] <- c(st$keys[[method]], new)
    st$keys_upto[[method]] <- n
  }
  st$keys[[method]]
}

# The part of the reference that could match the query (see R/genus_filter.R),
# in original reference order: the IDs and names of the records, and their
# match keys in the form the engine reads
.ts_ref_subset <- function(ref, query_parsed_df, max_dist_genus) {
  keep_keys <- .ts_genus_keep_keys(query_parsed_df, ref$genera, max_dist_genus)
  .ts_ref_fill(ref, keep_keys[keep_keys %in% ref$genera])
  st <- ref$state
  sel <- which(st$genus_key %in% keep_keys)
  sel <- sel[order(st$row[sel])]
  cache <- new.env(parent = emptyenv())
  keys <- list(
    code = st$code[sel],
    genus = st$genus[sel],
    fields = lapply(st$fields, function(x) x[sel]),
    no_auth = st$no_auth[sel],
    auct = st$auct[sel],
    key = function(method) {
      if (is.null(cache[[method]])) {
        assign(method, .ts_pool_key(st, method)[sel], envir = cache)
      }
      cache[[method]]
    }
  )
  list(ids = keys$code, names = ref$names[st$row[sel]], keys = keys)
}
