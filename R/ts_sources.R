#' List the named sources of references
#'
#' The sources that \code{\link{ts_load_ref}()} can download by name, with their
#' licenses and how to cite them. Check the license of a source before reusing
#' its data.
#'
#' By default no internet is used. With `versions = TRUE`, the versions that can
#' be downloaded are looked up (one or two requests per source), which is useful
#' to choose the `version` of \code{\link{ts_load_ref}()} to use. This can
#' take some time (a few seconds, and longer with a slow connection).
#'
#' @param versions Logical; also list the versions available to download?
#'   Needs internet. If a source cannot be reached, its versions are `NA` and a
#'   message says why. Default `FALSE`.
#' @param tidy Logical; return a "tidy" dataframe. If `versions` is `TRUE`,
#'   returns one row per version. Default `FALSE`.
#'
#' @return A dataframe with one row per source and these columns: `source` (the
#'   name to give to the `source` argument of \code{\link{ts_load_ref}()}),
#'   `name`, `description`, `license`, `citation` and `url`.
#'
#'   With `versions = TRUE`, the versions available to download for each source
#'   are shown, newest first. Printing the result shows only the latest
#'   versions of each source. To see all of them, also use `tidy = TRUE`.
#'
#'   With `tidy = TRUE`, the dataframe has one row per version, unless
#'   `versions = FALSE`.
#' @export
#' @examples
#' ts_show_ref_sources()
#'
#' \dontrun{
#' # Needs internet
#' ts_show_ref_sources(versions = TRUE)
#'
#' # One row per version
#' ts_show_ref_sources(versions = TRUE, tidy = TRUE)
#' }
ts_show_ref_sources <- function(versions = FALSE, tidy = FALSE) {
  assertthat::assert_that(assertthat::is.flag(versions), assertthat::noNA(versions))
  assertthat::assert_that(assertthat::is.flag(tidy), assertthat::noNA(tidy))
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
    .ts_state$taxadb_manifests <- NULL # look again for new versions
    errors <- character(0)
    out$versions <- lapply(srcs, function(s) {
      if (interactive()) .ts_inform("Looking up the versions of ", s$name, " ...")
      tryCatch(
        s$list_versions(),
        error = function(e) {
          errors[s$name] <<- conditionMessage(e)
          NA_character_
        }
      )
    })
    # one message per kind of failure (sources that share a lookup fail together)
    for (msg in unique(errors)) {
      .ts_inform(
        "Could not list the versions of ",
        paste(names(errors)[errors == msg], collapse = ", "), ": ", msg
      )
    }
    names(out$versions) <- names(srcs)
  }
  if (tidy && !versions) {
    return(out) # one row per source, and no versions to spread over rows
  }
  if (tidy) {
    n <- lengths(out$versions)
    long <- out[rep(seq_len(nrow(out)), n), colnames(out) != "versions", drop = FALSE]
    long$version <- unlist(out$versions, use.names = FALSE)
    rownames(long) <- NULL
    return(long)
  }
  class(out) <- c("ts_ref_sources", class(out))
  out
}

#' @export
print.ts_ref_sources <- function(x, ...) {
  cols <- c("source", "name", "description", "license", "citation", "url")
  if (!all(cols %in% colnames(x))) {
    return(NextMethod())
  }
  n_show <- 5
  for (i in seq_len(nrow(x))) {
    cat(x$source[i], ": ", x$name[i], "\n", sep = "")
    cat("  ", x$description[i], "\n", sep = "")
    cat("  License: ", x$license[i], "\n", sep = "")
    if ("versions" %in% colnames(x)) {
      v <- x$versions[[i]]
      line <- if (anyNA(v)) {
        "not available"
      } else if (length(v) > n_show) {
        paste0(
          paste(v[seq_len(n_show)], collapse = ", "), ", ... (", length(v),
          " in total)"
        )
      } else {
        paste(v, collapse = ", ")
      }
      cat("  Versions: ", line, "\n", sep = "")
    }
    cat("  Cite: ", x$citation[i], "\n", sep = "")
    cat("  More: ", x$url[i], "\n", sep = "")
    if (i < nrow(x)) cat("\n")
  }
  invisible(x)
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
      description = "Ferns and lycophytes",
      license = "CC BY 4.0",
      citation = paste0(
        "Pteridophyte Phylogeny Group. PPG taxonomic database. ",
        "https://doi.org/10.5281/zenodo.22760360"
      ),
      url = "https://github.com/pteridogroup/ppg",
      resolve = .ts_resolve_ppg,
      list_versions = .ts_versions_ppg,
      keep = NULL
    ),
    itis = .ts_taxadb_source(
      "itis", "Integrated Taxonomic Information System (ITIS)",
      "All kingdoms, mostly North American taxa", "CC0 1.0",
      "https://www.itis.gov/"
    ),
    col = .ts_taxadb_source(
      "col", "Catalogue of Life (COL)",
      "All kingdoms", "CC BY 4.0", "https://www.catalogueoflife.org/"
    ),
    gbif = .ts_taxadb_source(
      "gbif", "GBIF Backbone Taxonomy",
      "All kingdoms", "CC BY 4.0", "https://www.gbif.org/"
    ),
    ncbi = .ts_taxadb_source(
      "ncbi", "NCBI Taxonomy",
      "Organisms in the sequence databases", "CC0 1.0",
      "https://www.ncbi.nlm.nih.gov/taxonomy"
    ),
    ott = .ts_taxadb_source(
      "ott", "Open Tree Taxonomy (OTT)",
      "All kingdoms, as used by the Open Tree of Life", "CC0 1.0",
      "https://tree.opentreeoflife.org/"
    ),
    fb = .ts_taxadb_source(
      "fb", "FishBase", "Fishes", "CC BY-NC 4.0", "https://fishbase.org/"
    ),
    slb = .ts_taxadb_source(
      "slb", "SeaLifeBase", "Marine organisms other than fishes",
      "CC BY-NC 4.0", "https://www.sealifebase.org/"
    )
  )
}

# The taxadb snapshots (https://docs.ropensci.org/taxadb/) are Parquet files, one
# per provider, in a public folder of data.source.coop. Each version of the
# snapshots has a `manifest.csv` that describes the files.
.ts_taxadb_base <- "https://data.source.coop/cboettig/taxadb/"

.ts_taxadb_source <- function(provider, name, description, license, url) {
  list(
    name = name,
    description = paste0(description, " (via taxadb)"),
    license = license,
    citation = paste0(
      name, ", ", url, " Accessed through taxadb: Norman, K.E.A., ",
      "Chamberlain, S. & Boettiger, C. (2020) taxadb: A high-performance local ",
      "taxonomic database interface. Methods in Ecology and Evolution 11: ",
      "1153-1159. https://doi.org/10.1111/2041-210X.13440"
    ),
    url = url,
    resolve = function(version) .ts_resolve_taxadb(provider, version),
    list_versions = function() .ts_versions_taxadb(provider),
    keep = c(
      "taxonID", "scientificName", "scientificNameAuthorship", "taxonRank",
      "parentNameUsageID", "taxonomicStatus", "acceptedNameUsageID",
      "kingdom", "phylum", "class", "order", "family", "genus"
    )
  )
}

# Versions of the taxadb snapshots that have a manifest, as a named list of
# manifests (dataframes), newest first. Kept for the session, because it is
# needed for each provider; `refresh = TRUE` looks again.
.ts_state <- new.env(parent = emptyenv())

.ts_taxadb_manifests <- function(refresh = FALSE) {
  if (!refresh && !is.null(.ts_state$taxadb_manifests)) {
    return(.ts_state$taxadb_manifests)
  }
  xml <- .ts_fetch_text(
    "https://data.source.coop/cboettig?list-type=2&prefix=taxadb/&delimiter=/"
  )
  versions <- unique(sub(
    "^<Prefix>taxadb/([^/]*)/</Prefix>$", "\\1",
    regmatches(xml, gregexpr("<Prefix>taxadb/[^/<]*/</Prefix>", xml))[[1]]
  ))
  manifests <- list()
  for (v in versions) {
    m <- tryCatch(
      utils::read.csv(
        text = .ts_fetch_text(paste0(.ts_taxadb_base, v, "/manifest.csv")),
        stringsAsFactors = FALSE, na.strings = ""
      ),
      error = function(e) NULL
    )
    if (!is.null(m) && all(c("provider", "schema") %in% colnames(m))) {
      manifests[[v]] <- m
    }
  }
  if (length(manifests) == 0) {
    stop("Could not find any taxadb snapshots", call. = FALSE)
  }
  ord <- order(numeric_version(names(manifests), strict = FALSE), decreasing = TRUE)
  manifests <- manifests[ord]
  .ts_state$taxadb_manifests <- manifests
  manifests
}

# Versions of taxadb snapshots that have a Darwin Core table for the provider
# in a single Parquet file. (Older snapshots, such as 22.12, are split into many
# files; only the first would be read, so they are not offered.)
.ts_versions_taxadb <- function(provider) {
  manifests <- .ts_taxadb_manifests()
  has <- vapply(manifests, function(m) {
    parts <- if ("parts" %in% colnames(m)) m$parts else 1
    any(m$provider == provider & m$schema == "dwc" & parts == 1, na.rm = TRUE)
  }, logical(1))
  names(manifests)[has]
}

.ts_resolve_taxadb <- function(provider, version) {
  available <- .ts_versions_taxadb(provider)
  if (is.null(version)) version <- available[1]
  if (!version %in% available) {
    stop(
      "Version '", version, "' of the taxadb snapshots has no table for '",
      provider, "'. Versions you can use: ", paste(available, collapse = ", "),
      call. = FALSE
    )
  }
  m <- .ts_taxadb_manifests()[[version]]
  row <- m[m$provider == provider & m$schema == "dwc", , drop = FALSE][1, ]
  extra <- list()
  if ("upstream_version" %in% colnames(row) && !is.na(row$upstream_version)) {
    extra$upstream_version <- as.character(row$upstream_version)
  }
  list(
    version = version,
    url = paste0(.ts_taxadb_base, version, "/dwc_", provider, "_part_0.parquet"),
    extra = extra
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
      on.exit(close(con), add = TRUE)
      paste(readLines(con, warn = FALSE), collapse = "\n")
    },
    error = fail, warning = fail
  )
}
