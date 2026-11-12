#' @details
#' # Messages
#'
#' Functions give messages (and some warnings, such as for names that could not
#' be parsed) to tell you what happened. To turn them off for a whole session,
#' set the option `taxastand.verbosity`:
#'
#' ```
#' options(taxastand.verbosity = "quiet")
#' ```
#'
#' The default is `"inform"`. Errors and warnings about problems with your
#' input (for example, a `taxon` that was not found) are always given. To
#' silence a single call, use [suppressMessages()] or
#' [suppressWarnings()], or `withr::with_options()`.
#' @keywords internal
"_PACKAGE"
