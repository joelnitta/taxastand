#' Test if [taxon-tools](https://github.com/camwebb/taxon-tools) is installed
#'
#' @return `TRUE` if [taxon-tools](https://github.com/camwebb/taxon-tools) is
#'   installed, or `FALSE` if not.
#' @export
#'
#' @examples
#' ts_tt_installed()
ts_tt_installed <- function() {
  tryCatch(
    {
      parsenames_res <- processx::run("parsenames", "--version")
      matchnames_res <- processx::run("matchnames", "--version")
      return(TRUE)
    },
    error = function(error_message) {
      return(FALSE)
    }
  )
}
