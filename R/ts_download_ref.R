#' Download a reference and format it for taxastand
#'
#' Gets a table of taxonomic names in Darwin Core format from a URL or a local
#' file, optionally keeps only some taxa, and formats it with
#' \code{\link{ts_format_ref}()}. The result can be used directly as `ref` in
#' \code{\link{ts_match_names}()} and \code{\link{ts_resolve_names}()}, or
#' passed to \code{\link{ts_optimize_ref}()}.
#'
#' `source` can be a comma- or tab-separated text file (`.csv`, `.tsv`, `.txt`,
#' also compressed as `.gz`), or a zipped Darwin Core Archive (`.zip`). For a
#' Darwin Core Archive, the file with the taxa (the "core") and its columns are
#' found from the `meta.xml` file in the archive. All columns are read as text.
#'
#' Steps:
#'
#' 1. The file is downloaded (if `source` is a URL). Only HTTPS URLs are
#'    accepted. It is saved to a temporary file first and moved into place
#'    only when complete, so an interrupted download leaves nothing behind.
#' 2. The file is read.
#' 3. If `taxon` is given, \code{\link{ts_subset_ref}()} keeps only those taxa
#'    and their synonyms.
#' 4. The table is formatted with \code{\link{ts_format_ref}()}.
#'
#' Nothing is downloaded or written unless this function is called. Downloads
#' are kept in a folder for reuse: `cache_dir` if given, otherwise the folder
#' in the option `taxastand.cache_dir` if set, otherwise a folder in
#' [tempdir()], which is removed when R closes. To keep downloads between R
#' sessions, give a `cache_dir` (or set the option). If the file is already
#' there, it is used and not downloaded again, unless `refresh = TRUE`.
#'
#' The check that the data are what you expect is up to you: the
#' [dwctaxon](https://docs.ropensci.org/dwctaxon/) package can validate Darwin
#' Core taxonomic data. Check the license of the data before reusing it.
#'
#' @param source Character; an HTTPS URL, or the path to a file on your
#'   computer, of a table of taxonomic names (see Details).
#' @param version Optional character; a label for the version of the data, such
#'   as `"2026-06"`. It is not used to find the data; it is stored in the
#'   `ts_source` attribute of the result so the reference can be cited.
#' @param cache_dir Character; folder in which to keep downloaded files. Default
#'   `NULL` (see Details).
#' @param taxon,rank Optional; keep only these taxa (and their synonyms), as in
#'   \code{\link{ts_subset_ref}()}.
#' @param col_map,drop_ranks Passed to \code{\link{ts_subset_ref}()} (`col_map`)
#'   and \code{\link{ts_format_ref}()} (both).
#' @param refresh Logical; download again even if the file is already in
#'   `cache_dir`? Default `FALSE`.
#' @param quiet Logical; suppress the messages? Default `FALSE`.
#'
#' @return A dataframe as returned by \code{\link{ts_format_ref}()}, with the
#'   `ts_source` attribute describing where the data came from (the URL or
#'   file, `version`, and the date it was read).
#' @export
#' @examples
#' # Read a reference from a file (no download)
#' f <- tempfile(fileext = ".csv")
#' write.csv(
#'   data.frame(
#'     taxonID = c("1", "2"), acceptedNameUsageID = c("1", "1"),
#'     taxonomicStatus = c("accepted", "synonym"),
#'     scientificName = c("Polypodium vulgare L.", "Polypodium rotundatum Willd.")
#'   ),
#'   f,
#'   row.names = FALSE
#' )
#' ts_download_ref(f, version = "example")
#'
#' \dontrun{
#' # Download from a URL, and keep only one family; keep the file for next time
#' ref <- ts_download_ref(
#'   "https://example.org/taxonomy.zip",
#'   taxon = "Polypodiaceae",
#'   cache_dir = "~/taxastand-data"
#' )
#' }
ts_download_ref <- function(source, version = NULL, cache_dir = NULL,
                            taxon = NULL, rank = NULL, col_map = NULL,
                            drop_ranks = ts_ranks_above_species(),
                            refresh = FALSE, quiet = FALSE) {
  assertthat::assert_that(assertthat::is.string(source), !is.na(source))
  assertthat::assert_that(is.null(version) || assertthat::is.string(version))
  assertthat::assert_that(is.null(cache_dir) || assertthat::is.string(cache_dir))
  assertthat::assert_that(
    assertthat::is.flag(refresh), assertthat::noNA(refresh),
    assertthat::is.flag(quiet), assertthat::noNA(quiet)
  )

  is_url <- grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", source)
  if (is_url) {
    assertthat::assert_that(
      grepl("^https://", source),
      msg = "`source` must be an HTTPS URL (starting with 'https://') or a file path"
    )
    path <- .ts_cached_download(source, cache_dir, refresh, quiet)
  } else {
    assertthat::assert_that(
      file.exists(source), !dir.exists(source),
      msg = paste0("`source` is not an existing file: ", source)
    )
    path <- source
  }

  raw <- .ts_read_table(path)

  if (!is.null(taxon)) {
    raw <- ts_subset_ref(raw, taxon, rank = rank, col_map = col_map)
  }

  info <- list(
    source = source,
    version = version,
    retrieved = as.character(Sys.Date())
  )
  info <- info[!vapply(info, is.null, logical(1))]
  out <- ts_format_ref(
    raw,
    col_map = col_map, drop_ranks = drop_ranks, source = info
  )
  if (!quiet) {
    message(
      "Read ", nrow(out), " names from ", source,
      if (!is.null(version)) paste0(" (version ", version, ")"),
      "."
    )
  }
  out
}

# Folder used to keep downloads
.ts_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- getOption("taxastand.cache_dir")
  if (is.null(cache_dir)) cache_dir <- file.path(tempdir(), "taxastand")
  cache_dir <- path.expand(cache_dir)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}

# Download `url` into the cache (unless it is already there); return the path
.ts_cached_download <- function(url, cache_dir, refresh, quiet) {
  dir <- .ts_cache_dir(cache_dir)
  name <- basename(sub("[?#].*$", "", url))
  if (!nzchar(name)) name <- "download"
  # the hash keeps files with the same name from different URLs apart
  dest <- file.path(
    dir, paste0(substr(digest::digest(url, algo = "md5"), 1, 8), "-", name)
  )
  if (file.exists(dest) && !refresh) {
    if (!quiet) message("Using the file already downloaded: ", dest)
    return(dest)
  }
  if (!quiet) message("Downloading ", url)
  tmp <- tempfile(tmpdir = dir, fileext = ".part")
  on.exit(unlink(tmp), add = TRUE)
  ok <- tryCatch(
    {
      .ts_fetch(url, tmp)
      file.exists(tmp) && file.size(tmp) > 0
    },
    error = function(e) {
      stop(
        "Could not download ", url, ": ", conditionMessage(e),
        "\nCheck your internet connection and the URL.",
        call. = FALSE
      )
    }
  )
  if (!isTRUE(ok)) {
    stop("Could not download ", url, ": the file is empty.", call. = FALSE)
  }
  if (!file.rename(tmp, dest)) {
    stop("Could not save the download to ", dest, call. = FALSE)
  }
  dest
}

# Download `url` to `dest` with base R. Warnings (for example, failure to
# connect or an HTTP error) are turned into errors. Kept separate so tests can
# replace it without using the internet.
.ts_fetch <- function(url, dest) {
  old <- options(timeout = max(3600, getOption("timeout")))
  on.exit(options(old), add = TRUE)
  withCallingHandlers(
    utils::download.file(url, dest, mode = "wb", quiet = TRUE),
    warning = function(w) stop(conditionMessage(w), call. = FALSE)
  )
  invisible(dest)
}

# Read a table from a text file or a Darwin Core Archive (zip) as character
.ts_read_table <- function(path) {
  if (grepl("\\.zip$", path, ignore.case = TRUE)) {
    dir <- tempfile("dwca")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    tryCatch(
      utils::unzip(path, exdir = dir),
      error = function(e) stop("Could not unzip ", path, call. = FALSE),
      warning = function(w) stop("Could not unzip ", path, call. = FALSE)
    )
    return(.ts_read_dwca(dir, list.files(dir, recursive = TRUE)))
  }
  .ts_read_delim(path)
}

# Read a delimited text file; the delimiter is found from the first line unless
# given
.ts_read_delim <- function(path, sep = NULL, header = TRUE, skip = 0) {
  if (is.null(sep)) {
    first <- readLines(path, n = 1, warn = FALSE, encoding = "UTF-8")
    sep <- if (length(first) && lengths(regmatches(first, gregexpr("\t", first))) >
      lengths(regmatches(first, gregexpr(",", first)))) {
      "\t"
    } else {
      ","
    }
  }
  utils::read.table(
    path,
    sep = sep, header = header, skip = skip,
    quote = if (identical(sep, "\t")) "" else "\"",
    colClasses = "character", na.strings = c("", "NA"),
    comment.char = "", fill = TRUE, check.names = FALSE,
    stringsAsFactors = FALSE, fileEncoding = "UTF-8", strip.white = FALSE
  )
}

# Read the core table of an unzipped Darwin Core Archive. The files and columns
# come from meta.xml if there is one; otherwise the archive must hold a single
# table with a header.
.ts_read_dwca <- function(dir, files) {
  meta <- files[tolower(basename(files)) == "meta.xml"]
  if (length(meta) == 0) {
    tables <- files[grepl("\\.(csv|tsv|txt)$", files, ignore.case = TRUE)]
    if (length(tables) != 1) {
      stop(
        "The zip file has no meta.xml, and does not hold exactly one table",
        call. = FALSE
      )
    }
    return(.ts_read_delim(file.path(dir, tables)))
  }
  xml <- paste(readLines(file.path(dir, meta[1]), warn = FALSE), collapse = " ")
  core <- regmatches(xml, regexpr("<core\\b.*?</core>", xml, perl = TRUE))
  assertthat::assert_that(
    length(core) == 1,
    msg = "meta.xml in the zip file has no core table"
  )
  attr_of <- function(tag, name) {
    m <- regmatches(tag, regexpr(paste0(name, "=\"[^\"]*\""), tag))
    if (length(m) == 0) {
      return(NA_character_)
    }
    sub(paste0(name, "=\"([^\"]*)\""), "\\1", m)
  }
  core_tag <- regmatches(core, regexpr("<core\\b[^>]*>", core))
  sep <- attr_of(core_tag, "fieldsTerminatedBy")
  sep <- if (is.na(sep)) "," else gsub("\\\\t", "\t", sep)
  skip <- suppressWarnings(as.integer(attr_of(core_tag, "ignoreHeaderLines")))
  if (is.na(skip)) skip <- 0L
  loc <- sub("</location>", "", sub(".*<location>", "", regmatches(
    core, regexpr("<location>[^<]*</location>", core)
  )))
  file <- files[basename(files) == basename(loc)]
  assertthat::assert_that(
    length(file) >= 1,
    msg = paste0("The core file ", loc, " named in meta.xml is not in the zip file")
  )
  x <- .ts_read_delim(file.path(dir, file[1]), sep = sep, header = FALSE, skip = skip)

  # name the columns from the terms in meta.xml
  fields <- regmatches(core, gregexpr("<field\\b[^>]*>", core))[[1]]
  idx <- as.integer(vapply(fields, attr_of, "", name = "index"))
  term <- vapply(fields, attr_of, "", name = "term")
  term <- sub("^.*[/#]", "", term)
  nm <- paste0("V", seq_len(ncol(x)))
  ok <- !is.na(idx) & idx + 1 <= ncol(x)
  nm[idx[ok] + 1] <- term[ok]
  id_tag <- regmatches(core, regexpr("<id\\b[^>]*>", core))
  if (length(id_tag) == 1 && !"taxonID" %in% nm) {
    id_idx <- suppressWarnings(as.integer(attr_of(id_tag, "index")))
    if (!is.na(id_idx) && id_idx + 1 <= ncol(x) && grepl("^V[0-9]+$", nm[id_idx + 1])) {
      nm[id_idx + 1] <- "taxonID"
    }
  }
  colnames(x) <- make.unique(nm)
  x
}
