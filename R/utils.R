#' Make a dataframe with taxonomic names
#'
#' @param taxa Character vector; taxon names to be parsed by taxon-tools `parsenames`.
#' Missing values not allowed. Must all be unique.
#'
#' @return Dataframe with two columns: `id` and `name`
#' @keywords internal
#' @examples
#' \dontrun{
#' ts_make_name_df("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#' }
ts_make_name_df <- function(taxa) {

  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(assertthat::noNA(taxa), msg = "Input taxa may not contain NAs")
  assertthat::assert_that(all(assertr::is_uniq(taxa)), msg = "Input taxa must be unique")

  # Format input names as data frame with unique ID
  # ID is combination of first 8 chars of hash of the
  # input (taxa), followed by "-" and integer
  taxa_df <- data.frame(name = taxa)
  taxa_df$id <- 1:nrow(taxa_df)
  taxa_df$id <- paste(substr(digest::digest(taxa), 1, 8), taxa_df$id, sep = "-")

  taxa_df[, c("id", "name")]
}

#' Classify results of taxon-tools matching
#'
#' @param match_results Dataframe; output of tt_match_names()
#'
#' @return Dataframe with column `result_type` added
#' @keywords internal
#' @autoglobal
ts_classify_result <- function(match_results) {
  assertthat::assert_that(inherits(match_results, "data.frame"), msg = "match_results must be of class 'data.frame'")
  match_results %>%
    dplyr::add_count(query) %>%
    dplyr::mutate(
      result_type = dplyr::case_when(
        match_type != "no_match" & n == 1 ~ "single_match",
        match_type != "no_match" & n > 1 ~ "mult_match",
        match_type == "no_match" ~ "no_match",
        TRUE ~ NA_character_
      )
    ) %>%
    assertr::assert(assertr::not_na, result_type) %>%
    dplyr::select(-n)
}

# Helper function for tests: skip test if docker is not installed
skip_if_no_docker <- function() {
  if (babelwhale::test_docker_installation()) {
    return(invisible(TRUE))
  }
  testthat::skip("docker not installed")
}

# Helper function for tests: skip test if taxon-tools is not installed
skip_if_no_tt <- function() {
  if (ts_tt_installed()) {
    return(invisible(TRUE))
  }
  testthat::skip("taxon-tools not installed")
}

#' Run a containerised command with automatic mounting of files
#'
#' Similar to [run()], but automatically mounts files (and directories) so the
#' user doesn't have to keep track of volumes.
#'
#' The main difference to [run()] is that the use of names for the `args`; any
#' file (or directory) that should be mounted inside the container must be named
#' `file`. The other elements (arguments) don't need to be named. Note that it
#' is fine to have multiple elements with the same name (`file`).
#'
#' This should generally work as long as the command accepts absolute paths
#' for file input. If that is not the case, use [run()] instead and specify
#' paths and mounting manually.
#'
#' @inheritParams babelwhale::run
#' @param args Character vector, arguments to the command. Any files or
#'   directories that should be mounted must be named "file" (see example).
#' @param wd Local working directory to run command. If specified, the working
#'   directory will be mounted to the docker container.
#' @param wd_in_container Working directory to run command in
#'   the container. Defaults to the working directory mounted to the container
#'   (`wd`).
#'
#' @return List, formatted as output from [processx::run()]
#' @noRd
#' @examples
#' \dontrun{
#' if (test_docker_installation()) {
#'
#' # Count the number of lines in the DESCRIPTION and LICENSE
#' # files of this package
#' run_auto_mount(
#'   container_id = "alpine",
#'   command = "wc",
#'   args = c("-l",
#'     file = system.file("DESCRIPTION", package = "babelwhale"),
#'     file = system.file("LICENSE", package = "babelwhale")
#'   )
#' )
#'
#' }
#' }
run_auto_mount <- function(
  container_id,
  command,
  args = NULL,
  wd = NULL,
  wd_in_container = NULL,
  environment_variables = NULL,
  debug = FALSE,
  verbose = FALSE,
  stdout = "|",
  stderr = "|") {

  # Convert paths of file arguments to absolute for docker
  file_args <- args[names(args) == "file"]
  in_path <- fs::path_abs(file_args)
  in_file <- fs::path_file(in_path)
  in_dir <- fs::path_dir(in_path)

  # Make (most likely) unique prefix for folder name that
  # won't conflict with an existing folder in the container
  # based on the hash of the container id and command
  prefix <- digest::digest(c(container_id, command))

  # Specify volume mounting for working directory
  wd_volume <- NULL
  if (!is.null(wd)) {
    wd_path <- fs::path_abs(wd)
    if (is.null(wd_in_container)) wd_in_container <- glue::glue("/{prefix}_wd")
    wd_volume <- glue::glue("{wd_path}:{wd_in_container}")
  }

  # Specify all volumes: one per file, plus working directory
  volumes <- unique(
    c(
      glue::glue("{in_dir}:/{prefix}_{1:length(in_dir)}"),
      wd_volume
    ))

  # Replace file arg paths with location in container
  files_in_container <- glue::glue("/{prefix}_{1:length(in_dir)}/{in_file}")
  args[names(args) == "file"] <- files_in_container

  # Run docker via babelwhale
  babelwhale::run(
    container_id = container_id,
    command = command,
    args = args,
    volumes = volumes,
    workspace = wd_in_container,
    environment_variables = environment_variables,
    debug = debug,
    verbose = verbose,
    stdout = stdout,
    stderr = stderr
  )
}

