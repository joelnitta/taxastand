#' Test if taxon-tools is installed
#'
#' A message will be issued if taxon-tools is not installed.
#'
#' @return `TRUE` if taxon-tools is installed, or `FALSE` if not.
#' @export
#'
#' @examples
#' ts_tt_installed()
ts_tt_installed <- function(){
  tryCatch(
    {
      res <- processx::run("parsenames", "--version")
      return(TRUE)
    },
    error = function(error_message) {
      message("taxon-tools is not installed")
      return(FALSE)
    }
  )
}
