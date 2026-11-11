#' List the named sources of references
#'
#' The sources that \code{\link{ts_load_ref}()} can download by name, with their
#' licenses and how to cite them. Check the license of a source before reusing
#' its data.
#'
#' @return A dataframe with one row per source and columns `source` (the name
#'   to give to `ts_load_ref(source = )`), `name`, `description`, `license`,
#'   `citation` and `url`.
#' @export
#' @examples
#' ts_reference_sources()
ts_reference_sources <- function() {
  srcs <- .ts_sources()
  pick <- function(field) {
    vapply(srcs, function(s) s[[field]], character(1), USE.NAMES = FALSE)
  }
  data.frame(
    source = names(srcs),
    name = pick("name"),
    description = pick("description"),
    license = pick("license"),
    citation = pick("citation"),
    url = pick("url"),
    stringsAsFactors = FALSE
  )
}

# Registry of named sources. For each: descriptive fields, `resolve(version)`
# that returns the concrete `version` and the `url` of the file to download, and
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
      keep = NULL
    )
  )
}

# WFO is on Zenodo; every version is a record of the "concept" record 7460141.
# Only versions from 2022-12 on have the file `_DwC_backbone_R.zip`.
.ts_resolve_wfo <- function(version) {
  concept <- "7460141"
  recs <- .ts_zenodo_records(.ts_fetch_text(
    paste0("https://zenodo.org/api/records/", concept, "/versions/latest")
  ))
  if (!is.null(version) && !identical(recs$version[1], version)) {
    # the list of versions is found from the latest record (25 per page is
    # more than WFO has)
    recs <- .ts_zenodo_records(.ts_fetch_text(paste0(
      "https://zenodo.org/api/records/", recs$id[1], "/versions?size=25"
    )))
    recs <- recs[!is.na(recs$version) & recs$version == version, , drop = FALSE]
    if (nrow(recs) == 0) {
      stop(
        "WFO version '", version, "' was not found. Versions are named like ",
        "'2026-06'; see https://doi.org/10.5281/zenodo.", concept,
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
