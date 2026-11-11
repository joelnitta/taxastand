#' List the named sources of references
#'
#' The sources that \code{\link{ts_load_ref}()} can download by name, with their
#' licenses and how to cite them. Check the license of a source before reusing
#' its data.
#'
#' By default no internet is used. With `versions = TRUE`, the versions that can
#' be downloaded are looked up (one request per source), which is useful to
#' choose the `version` of \code{\link{ts_load_ref}()} to use.
#'
#' @param versions Logical; also list the versions available to download?
#'   Needs internet. If a source cannot be reached, its versions are `NA` and a
#'   message says why. Default `FALSE`.
#'
#' @return A dataframe with one row per source and columns `source` (the name
#'   to give to `ts_load_ref(source = )`), `name`, `description`, `license`,
#'   `citation` and `url`. With `versions = TRUE`, also `versions`: the
#'   versions available to download, newest first, separated by commas.
#' @export
#' @examples
#' ts_show_ref_sources()
#'
#' \dontrun{
#' # Needs internet
#' ts_show_ref_sources(versions = TRUE)
#' }
ts_show_ref_sources <- function(versions = FALSE) {
  assertthat::assert_that(assertthat::is.flag(versions), assertthat::noNA(versions))
  srcs <- .ts_sources()
  pick <- function(field) {
    vapply(srcs, function(s) s[[field]], character(1), USE.NAMES = FALSE)
  }
  out <- data.frame(
    source = names(srcs),
    name = pick("name"),
    description = pick("description"),
    license = pick("license"),
    citation = pick("citation"),
    url = pick("url"),
    stringsAsFactors = FALSE
  )
  if (versions) {
    out$versions <- vapply(srcs, function(s) {
      tryCatch(
        paste(s$list_versions(), collapse = ", "),
        error = function(e) {
          message("Could not list the versions of ", s$name, ": ", conditionMessage(e))
          NA_character_
        }
      )
    }, character(1), USE.NAMES = FALSE)
  }
  out
}

# Registry of named sources. For each: descriptive fields, `resolve(version)`
# that returns the concrete `version` and the `url` of the file to download,
# `list_versions()` that returns the downloadable versions (newest first), and
# `keep`, the columns to read (NULL for all).
.ts_sources <- function() {
  list(
    wfo = list(
      name = "World Flora Online Plant List",
      description = "Plants (vascular plants and bryophytes); a new version every June and December",
      license = "CC0 1.0",
      citation = paste0(
        "The World Flora Online Consortium. World Flora Online Plant List. ",
        "https://doi.org/10.5281/zenodo.7460141"
      ),
      url = "https://wfoplantlist.org/",
      resolve = .ts_resolve_wfo,
      list_versions = .ts_versions_wfo,
      keep = c(
        "taxonID", "scientificName", "scientificNameAuthorship", "taxonRank",
        "parentNameUsageID", "taxonomicStatus", "acceptedNameUsageID",
        "family", "subfamily", "tribe", "subtribe", "genus"
      )
    ),
    ppg = list(
      name = "Pteridophyte Phylogeny Group classification (PPG II)",
      description = "Ferns and lycophytes; a new version about every month",
      license = "CC BY 4.0",
      citation = paste0(
        "Pteridophyte Phylogeny Group. PPG taxonomic database. ",
        "https://doi.org/10.5281/zenodo.22760360. See also PPG I (2016) ",
        "https://doi.org/10.1111/jse.12229"
      ),
      url = "https://github.com/pteridogroup/ppg",
      resolve = .ts_resolve_ppg,
      list_versions = .ts_versions_ppg,
      keep = NULL
    )
  )
}

# WFO is on Zenodo; every version is a record of the "concept" record 7460141.
# Only versions from 2022-12 on have the file `_DwC_backbone_R.zip`.
.ts_resolve_wfo <- function(version) {
  recs <- .ts_wfo_records(latest_only = is.null(version))
  if (!is.null(version)) {
    recs <- recs[recs$version == version, , drop = FALSE]
    if (nrow(recs) == 0) {
      stop(
        "WFO version '", version, "' was not found. Versions you can use: ",
        paste(.ts_versions_wfo(), collapse = ", "),
        call. = FALSE
      )
    }
  }
  list(
    version = recs$version[1],
    url = paste0(
      "https://zenodo.org/records/", recs$id[1],
      "/files/_DwC_backbone_R.zip?download=1"
    )
  )
}

# Downloadable versions of WFO, newest first
.ts_versions_wfo <- function() {
  .ts_wfo_records(latest_only = FALSE)$version
}

# Records (id, version, date) of the WFO versions that can be downloaded.
.ts_wfo_records <- function(latest_only) {
  recs <- .ts_zenodo_records(.ts_fetch_text(
    "https://zenodo.org/api/records/7460141/versions/latest"
  ))
  if (!latest_only) {
    # the list of versions is found from the latest record (25 per page is
    # more than WFO has)
    recs <- .ts_zenodo_records(.ts_fetch_text(paste0(
      "https://zenodo.org/api/records/", recs$id[1], "/versions?size=25"
    )))
  }
  recs <- recs[
    !is.na(recs$version) & grepl("^[0-9]{4}-[0-9]{2}$", recs$version) &
      recs$version >= "2022-12", ,
    drop = FALSE
  ]
  recs[order(recs$version, decreasing = TRUE), , drop = FALSE]
}

# Get record ids and versions from the JSON of Zenodo records (without a JSON
# parser: each record starts with its "id" followed by "conceptrecid")
.ts_zenodo_records <- function(json) {
  starts <- as.integer(gregexpr('"id": *[0-9]+, *"conceptrecid"', json)[[1]])
  if (starts[1] == -1L) {
    stop("Could not read the version information from Zenodo", call. = FALSE)
  }
  ends <- c(starts[-1] - 1L, nchar(json))
  blocks <- substring(json, starts, ends)
  version <- ifelse(
    grepl('"version": *"', blocks),
    sub('^.*?"version": *"([^"]*)".*$', "\\1", blocks, perl = TRUE),
    NA_character_
  )
  data.frame(
    id = sub('^"id": *([0-9]+).*$', "\\1", blocks),
    version = version,
    stringsAsFactors = FALSE
  )
}

# PPG is a GitHub repository; versions are release tags
.ts_resolve_ppg <- function(version) {
  if (is.null(version)) {
    json <- .ts_fetch_text(
      "https://api.github.com/repos/pteridogroup/ppg/releases/latest"
    )
    m <- regmatches(json, regexpr('"tag_name": *"[^"]*"', json))
    if (length(m) == 0) {
      stop("Could not find the latest PPG release on GitHub", call. = FALSE)
    }
    version <- sub('^"tag_name": *"([^"]*)"$', "\\1", m)
  }
  list(
    version = version,
    url = paste0(
      "https://raw.githubusercontent.com/pteridogroup/ppg/", version,
      "/data/ppg.csv"
    )
  )
}

# Release tags of PPG, newest first
.ts_versions_ppg <- function() {
  json <- .ts_fetch_text(
    "https://api.github.com/repos/pteridogroup/ppg/releases?per_page=100"
  )
  tags <- regmatches(json, gregexpr('"tag_name": *"[^"]*"', json))[[1]]
  if (length(tags) == 0) {
    stop("Could not find any PPG releases on GitHub", call. = FALSE)
  }
  sub('^"tag_name": *"([^"]*)"$', "\\1", tags) # GitHub lists newest first
}

# Read the text at a URL (for small files, such as JSON)
.ts_fetch_text <- function(link) {
  fail <- function(e) {
    stop(
      "Could not get ", link, ": ", conditionMessage(e),
      "\nCheck your internet connection.",
      call. = FALSE
    )
  }
  tryCatch(
    {
      con <- url(link, encoding = "UTF-8")
      on.exit(close(con))
      paste(readLines(con, warn = FALSE), collapse = "\n")
    },
    error = fail, warning = fail
  )
}
