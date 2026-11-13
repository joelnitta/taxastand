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
#'   as `"wfo"` (World Flora Online) or `"itis"` (the Integrated Taxonomic
#'   Information System). See \code{\link{ts_show_ref_sources}()} for the
#'   list, licenses and how to cite them. The latest version is used unless you
#'   give a `version`, and the version that was used is recorded in the result.
#' - `url`: a direct link (HTTPS) to a file, not to a web page about it.
#' - `file`: the path to a file on your computer.
#'
#' A file can be a comma- or tab-separated text file (such as `.csv`, `.tsv` or
#' `.txt`, also compressed as `.gz`), a zipped Darwin Core Archive (`.zip`), or
#' a Parquet file (which needs the nanoparquet or the duckdb package; `"itis"`,
#' `"col"`, `"gbif"`, `"ncbi"`, `"ott"`, `"fb"` and `"slb"` are Parquet files
#' of the [taxadb](https://docs.ropensci.org/taxadb/) snapshots).
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
#' there, it is used and not downloaded again, unless `refresh = TRUE`. See
#' \code{\link{ts_show_ref_cache}()} and \code{\link{ts_clear_ref_cache}()} to
#' check and free the space. To silence the messages, set
#' `options(taxastand.verbosity = "quiet")` (see
#' \code{\link{taxastand-package}}).
#' Downloads from named sources can be large (World Flora Online is about
#' 120 MB, and GBIF about 360 MB), so loading can take a few minutes. Large
#' tables also need a lot of memory (a few GB for GBIF). For Parquet files, if
#' the duckdb package is installed and `taxon` is given, only the rows for
#' `taxon` and their synonyms are read, which is much faster and needs much less
#' memory; the result is the same.
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
#'   such as `"2026-06"` for `"wfo"` (versions from `"2022-12"` on) or
#'   `"v2.1.0"` for `"ppg"`; the default is the latest version (see
#'   `ts_show_ref_sources(versions = TRUE)` for the versions you can use). For
#'   `url` and `file`, a label for the version of the data, which is only
#'   stored in the `ts_source` attribute of the result so the reference can be
#'   cited.
#' @param cache_dir Character; folder in which to keep downloaded files. Default
#'   `NULL` (see Details).
#' @param taxon,rank Optional; keep only these taxa (and their synonyms), as in
#'   \code{\link{ts_subset_ref}()}.
#' @param col_map,drop_ranks Passed to \code{\link{ts_subset_ref}()} (`col_map`)
#'   and \code{\link{ts_format_ref}()} (both). Not needed for named sources.
#' @param refresh Logical; download again even if the file is already in
#'   `cache_dir`? Default `FALSE`.
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
#'     scientificName = c(
#'       "Polypodium vulgare L.", "Polypodium rotundatum Willd."
#'     )
#'   ),
#'   f,
#'   row.names = FALSE
#' )
#' ts_load_ref(file = f, version = "example")
#'
#' @examplesIf interactive()
#' # Download a named source (needs internet) and keep only one family. The
#' # file goes to a temporary folder unless you give `cache_dir`, where it is
#' # kept so it is not downloaded again next time.
#' ref <- ts_load_ref(source = "wfo", taxon = "Polypodiaceae")
#'
#' # A direct link (HTTPS) to a file works too:
#' # ts_load_ref(url = "https://example.org/taxonomy.zip")
ts_load_ref <- function(source = NULL, url = NULL, file = NULL, version = NULL,
                        cache_dir = NULL, taxon = NULL, rank = NULL,
                        col_map = NULL,
                        drop_ranks = ts_ranks_above_species(),
                        refresh = FALSE) {
  .ts_check_load_args(source, url, file, version, cache_dir, refresh)

  info <- list()
  keep <- NULL
  if (!is.null(source)) {
    spec <- .ts_load_source(source, version)
    version <- spec$version
    url <- spec$url
    keep <- spec$keep
    info <- spec$info
  }
  if (!is.null(url)) {
    path <- .ts_load_download(url, source, cache_dir, refresh)
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

  raw <- .ts_read_table(path, keep = keep, taxon = taxon, rank = rank)

  if (!is.null(taxon)) {
    raw <- ts_subset_ref(raw, taxon, rank = rank, col_map = col_map)
  }

  out <- ts_format_ref(
    raw,
    col_map = col_map, drop_ranks = drop_ranks, source = info
  )
  .ts_load_report(out, info, source, url, file)
  out
}

# Validate the arguments of ts_load_ref()
.ts_check_load_args <- function(source, url, file, version, cache_dir,
                                refresh) {
  given <- c(
    source = !is.null(source), url = !is.null(url),
    file = !is.null(file)
  )
  assertthat::assert_that(
    sum(given) == 1,
    msg = "Give exactly one of `source`, `url` or `file`"
  )
  assertthat::assert_that(
    is.null(source) || assertthat::is.string(source),
    is.null(url) || assertthat::is.string(url),
    is.null(file) || assertthat::is.string(file),
    is.null(version) || assertthat::is.string(version),
    is.null(cache_dir) || assertthat::is.string(cache_dir),
    assertthat::is.flag(refresh), assertthat::noNA(refresh)
  )
}

# Look up a named source: the version and the URL to download, the columns to
# keep, and the information to record in the result (`info`)
.ts_load_source <- function(source, version) {
  srcs <- .ts_sources()
  assertthat::assert_that(
    source %in% names(srcs),
    msg = paste0(
      "Unknown `source` '", source, "'. Choose one of: ",
      toString(names(srcs)),
      " (see ts_show_ref_sources()), or use `url` or `file`."
    )
  )
  spec <- srcs[[source]]
  .ts_state$taxadb_manifests <- NULL # look again for new versions
  resolved <- spec$resolve(version)
  list(
    version = resolved$version,
    url = resolved$url,
    keep = spec$keep,
    info = c(
      list(
        source = source, name = spec$name, license = spec$license,
        citation = spec$citation
      ),
      resolved$extra
    )
  )
}

# Download (or find in the cache) the file at `url`; return its path
.ts_load_download <- function(url, source, cache_dir, refresh) {
  assertthat::assert_that(
    grepl("^https://", url),
    msg = "`url` must be an HTTPS URL (starting with 'https://')"
  )
  tryCatch(
    .ts_cached_download(url, cache_dir, refresh),
    error = function(e) {
      hint <- if (!is.null(source)) {
        paste0(
          "\nTo see the versions you can use, run ",
          "ts_show_ref_sources(versions = TRUE)."
        )
      }
      stop(conditionMessage(e), hint, call. = FALSE)
    }
  )
}

# Say what was loaded; the citation is given once per R session for each source
.ts_load_report <- function(out, info, source, url, file) {
  from <- if (!is.null(info$name)) {
    info$name
  } else if (!is.null(url)) {
    url
  } else {
    file
  }
  .ts_inform(
    "Loaded ", nrow(out), " names from ", from,
    if (!is.null(info$version)) paste0(" (version ", info$version, ")"),
    if (!is.null(info$license)) paste0(", license ", info$license), "."
  )
  if (!is.null(source) && !is.null(info$citation) && !.ts_quiet() &&
    !source %in% .ts_state$cited) {
    message("Please cite: ", info$citation)
    .ts_state$cited <- c(.ts_state$cited, source)
  }
}

# Folder used to keep downloads
.ts_cache_path <- function(cache_dir = NULL) {
  if (is.null(cache_dir)) cache_dir <- getOption("taxastand.cache_dir")
  if (is.null(cache_dir)) cache_dir <- file.path(tempdir(), "taxastand")
  path.expand(cache_dir)
}

# Same, but make sure the folder exists
.ts_cache_dir <- function(cache_dir = NULL) {
  cache_dir <- .ts_cache_path(cache_dir)
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_dir
}

# Download `url` into the cache (unless it is already there); return the path
.ts_cached_download <- function(url, cache_dir, refresh) {
  dir <- .ts_cache_dir(cache_dir)
  name <- basename(sub("[?#].*$", "", url))
  if (!nzchar(name)) name <- "download"
  # the hash keeps files with the same name from different URLs apart
  dest <- file.path(
    dir, paste0(substr(digest::digest(url, algo = "md5"), 1, 8), "-", name)
  )
  if (file.exists(dest) && !refresh) {
    return(dest)
  }
  .ts_inform("Downloading ", url)
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

# Is the file a Parquet file? (they start with "PAR1")
.ts_is_parquet <- function(path) {
  identical(readBin(path, what = "raw", n = 4), charToRaw("PAR1"))
}

# Read a Parquet file as character columns (only the columns in `keep`, if
# given). With the duckdb package and a `taxon`, only the rows that
# ts_subset_ref() will keep are read (it is still run afterwards, so the result
# is the same); otherwise nanoparquet reads the whole file.
.ts_read_parquet <- function(path, keep = NULL, taxon = NULL, rank = NULL) {
  have_np <- .ts_have_package("nanoparquet")
  have_duck <- .ts_have_package("duckdb") && .ts_have_package("DBI")
  if (!have_np && !have_duck) {
    stop(
      "Reading a Parquet file needs the nanoparquet or the duckdb package. ",
      "Install one with install.packages(\"nanoparquet\").",
      call. = FALSE
    )
  }
  x <- NULL
  if (have_duck && (!is.null(taxon) || !have_np)) {
    x <- .ts_read_parquet_duckdb(path, keep, taxon, rank)
  }
  if (is.null(x) && !have_np) {
    x <- .ts_read_parquet_duckdb(path, keep, NULL, NULL) # subset in R later
  }
  if (is.null(x)) {
    schema <- nanoparquet::read_parquet_schema(path)
    cols <- schema$name[!is.na(schema$type)] # the first row is the file itself
    if (!is.null(keep)) cols <- intersect(cols, keep)
    x <- as.data.frame(
      nanoparquet::read_parquet(path, col_select = cols),
      stringsAsFactors = FALSE
    )
  }
  x[] <- lapply(x, function(v) {
    if (is.character(v)) {
      v[!is.na(v) & !nzchar(v)] <- NA_character_
      v
    } else {
      as.character(v)
    }
  })
  x
}

# Read a Parquet file with duckdb. If `taxon` is given and the file has columns
# for higher ranks, only the rows matching ts_subset_ref() are read: rows with
# `taxon` in one of those columns, their synonyms, and the accepted names of
# those synonyms. Returns NULL if `taxon` is given but cannot be used this way
# (the caller then reads everything and ts_subset_ref() does the work).
.ts_read_parquet_duckdb <- function(path, keep, taxon, rank) {
  args <- list()
  # do not create a folder in the home directory
  if ("shared_home" %in% names(formals(duckdb::duckdb))) {
    args$shared_home <- FALSE
  }
  con <- DBI::dbConnect(do.call(duckdb::duckdb, args))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbExecute(con, paste0(
    "SET temp_directory = ", DBI::dbQuoteString(con, tempfile("duckdb")), ";"
  ))
  # duckdb otherwise keeps a lot of the file in memory (GBIF: 2.3 GB, 1 GB with
  # this limit, in about the same time)
  DBI::dbExecute(con, "SET memory_limit = '1GB';")
  src <- paste0("read_parquet(", DBI::dbQuoteString(con, path), ")")
  all_cols <- DBI::dbGetQuery(
    con, paste0("DESCRIBE SELECT * FROM ", src)
  )$column_name
  cols <- if (is.null(keep)) all_cols else intersect(all_cols, keep)
  q <- function(x) as.character(DBI::dbQuoteIdentifier(con, x))
  select <- toString(q(cols))

  rank_cols <- intersect(subset_rank_columns(), all_cols)
  if (!is.null(rank)) rank_cols <- intersect(rank_cols, tolower(trimws(rank)))
  use_filter <- !is.null(taxon) && length(rank_cols) > 0 &&
    all(c("taxonID", "acceptedNameUsageID") %in% all_cols)
  if (!is.null(taxon) && !use_filter) {
    return(NULL)
  }
  if (!use_filter) {
    return(DBI::dbGetQuery(con, paste0("SELECT ", select, " FROM ", src)))
  }

  taxa <- toString(DBI::dbQuoteString(con, unique(trimws(taxon))))
  hit_where <- paste(paste0(q(rank_cols), " IN (", taxa, ")"),
    collapse = " OR "
  )
  DBI::dbGetQuery(con, paste0(
    "WITH base AS (SELECT ", select, " FROM ", src, "), ",
    "hit AS (SELECT * FROM base WHERE ", hit_where, "), ",
    "kept AS (SELECT * FROM hit UNION SELECT * FROM base WHERE ",
    q("acceptedNameUsageID"), " IN (SELECT ", q("taxonID"), " FROM hit)) ",
    "SELECT * FROM kept UNION SELECT * FROM base WHERE ",
    q("taxonID"), " IN (SELECT ", q("acceptedNameUsageID"), " FROM kept)"
  ))
}

.ts_have_package <- function(pkg) requireNamespace(pkg, quietly = TRUE)

# Read a table from a text file or a Darwin Core Archive (zip) as character
.ts_read_table <- function(path, keep = NULL, taxon = NULL, rank = NULL) {
  if (.ts_is_parquet(path)) {
    return(.ts_read_parquet(path, keep, taxon, rank))
  }
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
    n_tab <- lengths(regmatches(first, gregexpr("\t", first, fixed = TRUE)))
    n_comma <- lengths(regmatches(first, gregexpr(",", first, fixed = TRUE)))
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
  core_tag <- regmatches(core, regexpr("<core\\b[^>]*>", core))
  sep <- .ts_xml_attr(core_tag, "fieldsTerminatedBy")
  sep <- if (is.na(sep)) "," else gsub("\\t", "\t", sep, fixed = TRUE)
  skip <- suppressWarnings(
    as.integer(.ts_xml_attr(core_tag, "ignoreHeaderLines"))
  )
  if (is.na(skip)) skip <- 0L
  loc <- regmatches(core, regexpr("<location>[^<]*</location>", core))
  loc <- sub("</location>", "", sub(".*<location>", "", loc), fixed = TRUE)
  file <- files[basename(files) == basename(loc)]
  assertthat::assert_that(
    length(file) >= 1,
    msg = paste0(
      "The core file ", loc,
      " named in meta.xml is not in the zip file"
    )
  )
  x <- .ts_read_delim(file.path(dir, file[1]),
    sep = sep, header = FALSE,
    skip = skip
  )

  colnames(x) <- .ts_dwca_colnames(core, ncol(x))
  x
}

# The value of the attribute `name` in the XML `tag`, NA if it has none
.ts_xml_attr <- function(tag, name) {
  m <- regmatches(tag, regexpr(paste0(name, "=\"[^\"]*\""), tag))
  if (length(m) == 0) {
    return(NA_character_)
  }
  sub(paste0(name, "=\"([^\"]*)\""), "\\1", m)
}

# Name the `n` columns of the core table from the terms in meta.xml (`core` is
# the text of its core element). The id column is the taxonID unless a field
# says that it is something else.
.ts_dwca_colnames <- function(core, n) {
  fields <- regmatches(core, gregexpr("<field\\b[^>]*>", core))[[1]]
  idx <- as.integer(vapply(fields, .ts_xml_attr, "", name = "index"))
  term <- vapply(fields, .ts_xml_attr, "", name = "term")
  term <- sub("^.*[/#]", "", term)
  nm <- paste0("V", seq_len(n))
  ok <- !is.na(idx) & idx + 1 <= n
  nm[idx[ok] + 1] <- term[ok]
  id_tag <- regmatches(core, regexpr("<id\\b[^>]*>", core))
  if (length(id_tag) == 1 && !"taxonID" %in% nm) {
    id_idx <- suppressWarnings(as.integer(.ts_xml_attr(id_tag, "index")))
    if (!is.na(id_idx) && id_idx + 1 <= n && grepl(
      "^V[0-9]+$",
      nm[id_idx + 1]
    )) {
      nm[id_idx + 1] <- "taxonID"
    }
  }
  make.unique(nm)
}
