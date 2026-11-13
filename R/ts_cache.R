#' Show or clear the files downloaded by ts_load_ref()
#'
#' `ts_show_ref_cache()` lists the files that \code{\link{ts_load_ref}()} has
#' downloaded; `ts_clear_ref_cache()` deletes them. Neither uses the internet.
#'
#' The folder is `cache_dir` if given, otherwise the folder in the option
#' `taxastand.cache_dir` if set, otherwise a folder in [tempdir()] (removed when
#' R closes). Only files made by `ts_load_ref()` (names like
#' `1a2b3c4d-name.zip`, and `.part` files left by a failed download) are
#' listed or deleted; other files in the folder are never touched.
#'
#' @param cache_dir Character; the folder to look in. Default `NULL`.
#' @param older_than Numeric; only delete files last modified more than this
#'   many days ago. Default `NULL` (delete all).
#' @param ask Logical; ask before deleting? Default `TRUE` in an interactive
#'   session. There is never a prompt when not interactive.
#'
#' @return `ts_show_ref_cache()` returns a dataframe with one row per file: the
#'   original `file` name, `size` (such as "121.7 MB"), `modified` and `path`.
#'   It is empty if there is nothing in the folder. `ts_clear_ref_cache()`
#'   invisibly returns the paths of the deleted files.
#' @export
#' @examples
#' ts_show_ref_cache()
#' ts_clear_ref_cache()
ts_show_ref_cache <- function(cache_dir = NULL) {
  assertthat::assert_that(
    is.null(cache_dir) || assertthat::is.string(cache_dir)
  )
  dir <- .ts_cache_path(cache_dir)
  paths <- .ts_cache_files(dir)
  info <- file.info(paths)
  out <- data.frame(
    file = sub("^[0-9a-f]{8}-", "", basename(paths)),
    size = .ts_size(info$size),
    modified = info$mtime,
    path = paths,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$file), , drop = FALSE]
  rownames(out) <- NULL
  if (nrow(out) == 0) {
    .ts_inform("No downloaded files in ", dir)
  } else {
    .ts_inform("Downloaded files in ", dir)
  }
  out
}

#' @rdname ts_show_ref_cache
#' @export
ts_clear_ref_cache <- function(cache_dir = NULL, older_than = NULL,
                               ask = interactive()) {
  assertthat::assert_that(
    is.null(cache_dir) || assertthat::is.string(cache_dir),
    is.null(older_than) ||
      (is.numeric(older_than) && length(older_than) == 1 && !is.na(older_than)),
    assertthat::is.flag(ask), !is.na(ask)
  )
  dir <- .ts_cache_path(cache_dir)
  paths <- .ts_cache_files(dir)
  if (!is.null(older_than)) {
    age <- as.numeric(difftime(Sys.time(), file.info(paths)$mtime,
      units = "days"
    ))
    paths <- paths[age > older_than]
  }
  if (length(paths) == 0) {
    .ts_inform("Nothing to delete in ", dir)
    return(invisible(character(0)))
  }
  size <- .ts_size(sum(file.size(paths)))
  if (ask) {
    ok <- utils::askYesNo(
      paste0("Delete ", length(paths), " file(s) (", size, ") in ", dir, "?"),
      default = FALSE
    )
    if (!isTRUE(ok)) {
      .ts_inform("Nothing deleted.")
      return(invisible(character(0)))
    }
  }
  done <- paths[file.remove(paths)]
  .ts_inform("Deleted ", length(done), " file(s), freeing ", size, ".")
  invisible(done)
}

# Files in `dir` that ts_load_ref() made: "<8 hex>-<name>" and "*.part"
.ts_cache_files <- function(dir) {
  if (!dir.exists(dir)) {
    return(character(0))
  }
  f <- list.files(dir, full.names = TRUE)
  f <- f[!dir.exists(f)]
  f[grepl("^[0-9a-f]{8}-.", basename(f)) | grepl("\\.part$", basename(f))]
}

# Human-readable file sizes, such as "121.7 MB"
.ts_size <- function(bytes) {
  vapply(
    bytes,
    function(b) {
      format(structure(b, class = "object_size"),
        units = "auto",
        standard = "SI"
      )
    },
    character(1)
  )
}
