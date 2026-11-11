#' Load a reference from a named source, a URL or a file
#'
#' Gets a table of taxonomic names in Darwin Core format, by downloading it
#' (from a named source or a URL) or reading it from a local file, optionally
#' keeps only some taxa, and formats it with \code{\link{ts_format_ref}()}. The
#' result can be used directly as `ref` in \code{\link{ts_match_names}()} and
#' \code{\link{ts_resolve_names}()}, or passed to
#' \code{\link{ts_optimize_ref}()}.
#'
#' Give the data in one of three ways, and only one:
#'
#' - `source`: the name of a source that taxastand knows how to download, such
#'   as `"wfo"` (World Flora Online) or `"ppg"` (Pteridophyte Phylogeny Group).
#'   See \code{\link{ts_show_ref_sources}()} for the list, licenses and how to
#'   cite them. The latest version is used unless you give a `version`, and the
#'   version that was used is recorded in the result.
#' - `url`: a direct link (HTTPS) to a file, not to a web page about it.
#' - `file`: the path to a file on your computer.
#'
#' A file can be a comma- or tab-separated text file (such as `.csv`, `.tsv` or
#' `.txt`, also compressed as `.gz`), or a zipped Darwin Core Archive (`.zip`).
#' The type is found from the contents of the file, not its name. For a Darwin
#' Core Archive, the file with the taxa (the "core") and its columns are found
#' from the `meta.xml` file in the archive. All columns are read as text.
#'
#' Steps:
#'
#' 1. The file is downloaded (for `source` and `url`). It is saved to a
#'    temporary file first and moved into place only when complete, so an
#'    interrupted download leaves nothing behind.
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
#' Downloads from named sources can be large (World Flora Online is about
#' 120 MB), so loading can take a few minutes.
#'
#' Checking that the data are what you expect is up to you: the
#' [dwctaxon](https://docs.ropensci.org/dwctaxon/) package can validate Darwin
#' Core taxonomic data. Check the license of the data before reusing it.
#'
#' @param source Character; name of a source to download (see Details and
#'   \code{\link{ts_show_ref_sources}()}).
#' @param url Character; HTTPS URL of a file. It must be a direct link to the
#'   file itself, not to a web page about it. Links that redirect to the file
#'   are fine.
#' @param file Character; path to a file.
#' @param version Optional character. For `source`, the version to download,
#'   such as `"2026-06"` for `"wfo"` (versions from `"2022-12"` on) or `"v2.1.0"`
#'   for `"ppg"`; the default is the latest version. For `url` and `file`, a label for the version of the
#'   data, which is only stored in the `ts_source` attribute of the result so
#'   the reference can be cited.
#' @param cache_dir Character; folder in which to keep downloaded files. Default
#'   `NULL` (see Details).
#' @param taxon,rank Optional; keep only these taxa (and their synonyms), as in
#'   \code{\link{ts_subset_ref}()}.
#' @param col_map,drop_ranks Passed to \code{\link{ts_subset_ref}()} (`col_map`)
#'   and \code{\link{ts_format_ref}()} (both). Not needed for named sources.
#' @param refresh Logical; download again even if the file is already in
#'   `cache_dir`? Default `FALSE`.
#' @param quiet Logical; suppress the messages? Default `FALSE`.
#'
#' @return A dataframe as returned by \code{\link{ts_format_ref}()}, with the
#'   `ts_source` attribute describing where the data came from (the source,
#'   URL or file, version, license and citation if known, and the date it was
#'   read).
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
#' ts_load_ref(file = f, version = "example")
#'
#' \dontrun{
#' # Download a named source and keep only one family. The file is kept in
#' # `cache_dir` so it is not downloaded again next time.
#' ref <- ts_load_ref(
#'   source = "wfo",
#'   taxon = "Polypodiaceae",
#'   cache_dir = "~/taxastand-data"
#' )
#'
#' # Download a file from a URL
#' ref <- ts_load_ref(url = "https://example.org/taxonomy.zip")
#' }
ts_load_ref <- function(source = NULL, url = NULL, file = NULL, version = NULL,
                        cache_dir = NULL, taxon = NULL, rank = NULL,
                        col_map = NULL,
                        drop_ranks = ts_ranks_above_species(),
                        refresh = FALSE, quiet = FALSE) {
  given <- c(source = !is.null(source), url = !is.null(url), file = !is.null(file))
  assertthat::assert_that(
    sum(given) == 1,
    msg = "Give exactly one of `source`, `url` and `file`"
  )
  assertthat::assert_that(is.null(source) || assertthat::is.string(source))
  assertthat::assert_that(is.null(url) || assertthat::is.string(url))
  assertthat::assert_that(is.null(file) || assertthat::is.string(file))
  assertthat::assert_that(is.null(version) || assertthat::is.string(version))
  assertthat::assert_that(is.null(cache_dir) || assertthat::is.string(cache_dir))
  assertthat::assert_that(
    assertthat::is.flag(refresh), assertthat::noNA(refresh),
    assertthat::is.flag(quiet), assertthat::noNA(quiet)
  )

  info <- list()
  keep <- NULL
  if (!is.null(source)) {
    srcs <- .ts_sources()
    assertthat::assert_that(
      source %in% names(srcs),
      msg = paste0(
        "Unknown `source` '", source, "'. Choose one of: ",
        paste(names(srcs), collapse = ", "),
        " (see ts_show_ref_sources()), or use `url` or `file`."
      )
    )
    spec <- srcs[[source]]
    resolved <- spec$resolve(version)
    version <- resolved$version
    url <- resolved$url
    keep <- spec$keep
    info <- list(
      source = source, name = spec$name, license = spec$license,
      citation = spec$citation
    )
  }
  if (!is.null(url)) {
    assertthat::assert_that(
      grepl("^https://", url),
      msg = "`url` must be an HTTPS URL (starting with 'https://')"
    )
    path <- .ts_cached_download(url, cache_dir, refresh, quiet)
    info$url <- url
  } else {
    assertthat::assert_that(
      file.exists(file), !dir.exists(file),
      msg = paste0("`file` is not an existing file: ", file)
    )
    path <- file
    info$file <- file
  }
  info$version <- version
  info$retrieved <- as.character(Sys.Date())

  raw <- .ts_read_table(path, keep = keep)

  if (!is.null(taxon)) {
    raw <- ts_subset_ref(raw, taxon, rank = rank, col_map = col_map)
  }

  out <- ts_format_ref(
    raw,
    col_map = col_map, drop_ranks = drop_ranks, source = info
  )
  if (!quiet) {
    from <- if (!is.null(info$name)) info$name else if (!is.null(url)) url else file
    message(
      "Loaded ", nrow(out), " names from ", from,
      if (!is.null(version)) paste0(" (version ", version, ")"), "."
    )
    if (!is.null(info$license)) {
      message(
        "License: ", info$license, ". Please cite: ", info$citation
      )
    }
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

# Is the file a zip file? Zip files start with the bytes "PK" followed by 3, 4
# (or 5, 6 if the archive is empty).
.ts_is_zip <- function(path) {
  sig <- readBin(path, what = "raw", n = 4)
  length(sig) == 4 && sig[1] == as.raw(0x50) && sig[2] == as.raw(0x4b) &&
    sig[3] %in% as.raw(c(0x03, 0x05))
}

# Read a table from a text file or a Darwin Core Archive (zip) as character
.ts_read_table <- function(path, keep = NULL) {
  if (.ts_is_zip(path)) {
    dir <- tempfile("dwca")
    dir.create(dir)
    on.exit(unlink(dir, recursive = TRUE), add = TRUE)
    tryCatch(
      utils::unzip(path, exdir = dir),
      error = function(e) stop("Could not unzip ", path, call. = FALSE),
      warning = function(w) stop("Could not unzip ", path, call. = FALSE)
    )
    return(.ts_read_dwca(dir, list.files(dir, recursive = TRUE), keep))
  }
  .ts_read_delim(path, keep = keep)
}

# Read a delimited text file as character; the delimiter is found from the first
# line unless given. If `keep` is given, only those columns are read (this saves
# a lot of memory for large files). In tab-separated files quotes are not
# interpreted, because some databases have stray quotes in free-text columns;
# quotes wrapped around a whole value are removed instead.
.ts_read_delim <- function(path, sep = NULL, header = TRUE, skip = 0,
                           keep = NULL) {
  first <- readLines(path, n = skip + 1, warn = FALSE, encoding = "UTF-8")
  first <- first[length(first)]
  if (is.null(sep)) {
    n_tab <- lengths(regmatches(first, gregexpr("\t", first)))
    n_comma <- lengths(regmatches(first, gregexpr(",", first)))
    sep <- if (length(first) && n_tab > n_comma) "\t" else ","
  }
  tab <- identical(sep, "\t")
  col_classes <- "character"
  if (!is.null(keep) && header && length(first)) {
    nm <- strsplit(first, sep, fixed = TRUE)[[1]]
    nm <- sub('^"(.*)"$', "\\1", nm)
    col_classes <- ifelse(nm %in% keep, "character", "NULL")
  }
  x <- utils::read.table(
    path,
    sep = sep, header = header, skip = skip,
    quote = if (tab) "" else "\"",
    colClasses = col_classes, na.strings = c("", "NA"),
    comment.char = "", fill = TRUE, check.names = FALSE,
    stringsAsFactors = FALSE, encoding = "UTF-8", strip.white = FALSE
  )
  if (tab) {
    # useBytes: some files have invalid UTF-8 in free-text columns
    x[] <- lapply(x, function(v) sub('^"(.*)"$', "\\1", v, useBytes = TRUE))
  }
  x
}

# Read the core table of an unzipped Darwin Core Archive. The files and columns
# come from meta.xml if there is one; otherwise the archive must hold a single
# table with a header.
.ts_read_dwca <- function(dir, files, keep = NULL) {
  meta <- files[tolower(basename(files)) == "meta.xml"]
  if (length(meta) == 0) {
    tables <- files[grepl("\\.(csv|tsv|txt)$", files, ignore.case = TRUE)]
    if (length(tables) != 1) {
      stop(
        "The zip file has no meta.xml, and does not hold exactly one table",
        call. = FALSE
      )
    }
    return(.ts_read_delim(file.path(dir, tables), keep = keep))
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
