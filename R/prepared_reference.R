#' Prepare a reference for repeated matching
#'
#' Matching names against a reference spends most of its time preparing the
#' reference: finding the genus of every name, parsing the names and building
#' the keys used for comparison. `ts_prepare_reference()` does the first of
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
#' @param reference Character vector of reference names (with authors), or a
#'   dataframe of taxonomic data in Darwin Core format (see
#'   \code{\link{ts_resolve_names}()}). A dataframe can be used for both
#'   \code{\link{ts_match_names}()} and \code{\link{ts_resolve_names}()} (as
#'   `ref_taxonomy`); a character vector only for the former.
#' @param strip_qualifiers Logical; remove standalone open-nomenclature
#'   qualifiers (`cf.`, `aff.`, ...) from the reference names before parsing.
#'   See \code{\link{ts_parse_names}()}. Default: `FALSE`. The setting used
#'   here applies to the reference; the `strip_qualifiers` argument of the
#'   matching functions then applies to the query only.
#' @return A `ts_reference` object.
#' @export
#' @examples
#' data(filmy_taxonomy)
#' ref <- ts_prepare_reference(filmy_taxonomy)
#' ref
#' # Reuse for several calls
#' ts_match_names("Gonocormus minutum", ref, simple = TRUE)
#' ts_resolve_names("Gonocormus minutum", ref)
ts_prepare_reference <- function(reference, strip_qualifiers = FALSE) {
  assertthat::assert_that(assertthat::is.flag(strip_qualifiers))
  taxonomy <- NULL
  if (inherits(reference, "data.frame")) {
    assertthat::assert_that(
      "scientificName" %in% colnames(reference),
      msg = "reference must have a `scientificName` column"
    )
    cols <- intersect(
      c("taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"),
      colnames(reference)
    )
    taxonomy <- as.data.frame(reference)[, cols, drop = FALSE]
    reference <- unique(as.character(reference$scientificName))
  }
  assertthat::assert_that(
    is.character(reference),
    msg = "reference must be a character vector or data.frame"
  )
  assertthat::assert_that(
    !anyNA(reference),
    msg = "reference must not contain missing values"
  )
  .ts_new_reference(reference, strip_qualifiers, taxonomy)
}

#' @export
print.ts_reference <- function(x, ...) {
  cat("<ts_reference>", length(x$names), "names in", length(x$genera), "genera\n")
  cat(
    "  genera parsed so far:", length(ls(x$cache, all.names = TRUE)), "\n"
  )
  invisible(x)
}

# ---- Internals ----------------------------------------------------------------

# Build the (cheap) skeleton: raw names, genus key of each, and an empty
# per-genus cache that is filled as queries need genera.
.ts_new_reference <- function(names, strip_qualifiers = FALSE, taxonomy = NULL) {
  genus_key <- .ts_raw_genus_key(names)
  structure(
    list(
      names = names,
      genus_key = genus_key,
      genera = unique(genus_key),
      strip_qualifiers = strip_qualifiers,
      taxonomy = taxonomy,
      cache = new.env(parent = emptyenv())
    ),
    class = "ts_reference"
  )
}

# Make sure the genera `keys` are parsed and keyed in the cache. All genera
# that are not cached yet are parsed in one batch, then split by genus.
.ts_ref_fill <- function(ref, keys) {
  cold <- keys[!vapply(keys, exists, NA, envir = ref$cache, inherits = FALSE)]
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
  # back to their position in the reference. The ID encodes that position,
  # which also restores the original order of records when genera combine.
  rows <- rows[match(parsed$name, ref$names[rows])]
  parsed$id <- sprintf("ref-%d", rows)
  lines <- .ts_parsed_lines(parsed, rows)
  by_genus <- split(seq_along(rows), ref$genus_key[rows])
  for (key in setdiff(cold, names(by_genus))) {
    by_genus[[key]] <- integer()
  }
  for (key in names(by_genus)) {
    i <- by_genus[[key]]
    assign(
      key,
      list(rows = rows[i], parsed = parsed[i, ], keys = .tt_record_keys(lines[i])),
      envir = ref$cache
    )
  }
  invisible()
}

# Parsed names and match keys of the reference restricted to the genera that
# could match the query (see R/genus_filter.R), in original reference order
.ts_ref_subset <- function(ref, query_parsed_df, max_dist_genus) {
  keep_keys <- .ts_genus_keep_keys(query_parsed_df, ref$genera, max_dist_genus)
  keep_keys <- keep_keys[keep_keys %in% ref$genera]
  .ts_ref_fill(ref, keep_keys)
  parts <- lapply(keep_keys, get, envir = ref$cache, inherits = FALSE)
  parts <- parts[lengths(lapply(parts, `[[`, "rows")) > 0L]
  .ts_merge_parts(parts)
}

# Combine per-genus entries into one parsed data frame and one keys object, with
# records in original reference order
.ts_merge_parts <- function(parts) {
  if (!length(parts)) {
    return(list(parsed = .ts_empty_parsed_df(), keys = .tt_record_keys(character())))
  }
  if (length(parts) == 1L) {
    return(list(parsed = parts[[1]]$parsed, keys = parts[[1]]$keys))
  }
  rows <- unlist(lapply(parts, `[[`, "rows"), use.names = FALSE)
  ord <- order(rows)
  parsed <- do.call(rbind, lapply(parts, `[[`, "parsed"))[ord, , drop = FALSE]
  rownames(parsed) <- NULL
  keys <- lapply(parts, `[[`, "keys")
  gather <- function(get) unlist(lapply(keys, get), use.names = FALSE)[ord]
  cache <- new.env(parent = emptyenv())
  field_names <- names(keys[[1]]$fields)
  list(
    parsed = parsed,
    keys = list(
      code = gather(function(k) k$code),
      genus = gather(function(k) k$genus),
      fields = stats::setNames(
        lapply(field_names, function(f) gather(function(k) k$fields[[f]])),
        field_names
      ),
      no_auth = gather(function(k) k$no_auth),
      auct = gather(function(k) k$auct),
      key = function(method) {
        if (is.null(cache[[method]])) {
          assign(method, gather(function(k) k$key(method)), envir = cache)
        }
        cache[[method]]
      }
    )
  )
}
