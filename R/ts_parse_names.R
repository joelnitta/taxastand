#' Parse taxonomic names
#'
#' Requires [taxon-tools](https://github.com/camwebb/taxon-tools) or docker
#' to be installed.
#'
#' Parses scientific names into their component parts (genus, species, variety, author, etc).
#'
#' @param taxa Character vector; taxon names to be parsed by taxon-tools `parsenames`.
#' Missing values not allowed. Must all be unique.
#' @param tbl_out Logical vector of length 1; should a tibble be returned?
#' If `FALSE` (default), output will be a data.frame. This argument can
#' be controlled via the option `ts_tbl_out`; see Examples.
#' @param quiet Logical; if TRUE, suppress warning messages that would normally
#' be issued
#' @param docker Logical; if TRUE, docker will be used to run taxon-tools
#' (so that taxon-tools need not be installed).
#'
#' @return A dataframe including the following columns.
#' - id: A unique ID number assigned to the input name
#' - name: The input name
#' - genus_hybrid_sign: Hybrid sign for genus
#' - genus_name: Genus name
#' - species_hybrid_sign: Hybrid sign for species
#' - specific_epithet: Specific epithet (name)
#' - infraspecific_rank: Infraspecific rank
#' - infraspecific_epithet: Infraspecific epithet (name)
#' - author: Name of taxon
#'
#' @autoglobal
#' @export
#' @examples
#' # Using local taxon-tools installation
#' if (ts_tt_installed()) {
#'
#'   ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#'   ts_parse_names(
#'     "Foogenus x barspecies var. foosubsp (L.) F. Bar", tbl_out = TRUE)
#'
#'   # If you always want tibble output without specifying `tbl_out = TRUE`
#'   # every time, set the option:
#'   options(ts_tbl_out = TRUE)
#'   ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#'   ts_parse_names("Crepidomanes minutum (Blume) K. Iwats.")
#'
#' }
#'
#' # Using docker
#' if (babelwhale::test_docker_installation()) {
#'
#' ts_parse_names(
#'   "Foogenus x barspecies var. foosubsp (L.) F. Bar",
#'   docker = TRUE)
#'
#' }
#'
ts_parse_names <- function(
  taxa,
  tbl_out = getOption("ts_tbl_out", default = FALSE),
  quiet = FALSE,
  docker = getOption("ts_docker", default = FALSE)
) {
  # Check input: must be character vector, no NA values, all unique
  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(
    assertthat::noNA(taxa),
    msg = "Input taxa may not contain NAs"
  )
  assertthat::assert_that(
    all(assertr::is_uniq(taxa)),
    msg = "Input taxa must be unique"
  )
  assertthat::assert_that(assertthat::is.flag(tbl_out))
  assertthat::assert_that(assertthat::is.flag(docker))

  # Write out names formatted for parsing with taxon-tools to temp file
  # format:
  # `id_num|taxon_name`
  # for example,
  # `x-234|Foogenus x barspecies var. foosubsp (L.) F. Bar`
  taxa_tbl <- ts_make_name_df(taxa)
  taxa_tbl$record <- paste(taxa_tbl$id, taxa_tbl$name, sep = "|")
  ref_taxa_txt_file <- tempfile(
    pattern = digest::digest(taxa),
    fileext = ".txt"
  )
  if (fs::file_exists(ref_taxa_txt_file)) fs::file_delete(ref_taxa_txt_file)
  writeLines(taxa_tbl$record, ref_taxa_txt_file)

  # Parse reference names with taxon tools
  if (isTRUE(docker)) {
    assertthat::assert_that(
      requireNamespace("babelwhale", quietly = TRUE),
      msg = "babelwhale needs to be installed to use docker"
    )
    assertthat::assert_that(
      babelwhale::test_docker_installation(),
      msg = "docker not installed"
    )
    ref_parsed <- run_auto_mount(
      container_id = "camwebb/taxon-tools:v1.3.0",
      command = "parsenames",
      args = c(file = ref_taxa_txt_file)
    )
  } else {
    assertthat::assert_that(
      ts_tt_installed(),
      msg = "taxon-tools not installed"
    )
    ref_parsed <- processx::run("parsenames", ref_taxa_txt_file)
  }

  if (fs::file_exists(ref_taxa_txt_file)) fs::file_delete(ref_taxa_txt_file)

  # Read in results of parsing, format as dataframe

  # The output is originally one record per line, with fields separated by '|' (pipe symbol)
  parsed_names <- data.frame(
    record = strsplit(ref_parsed[["stdout"]], "\n")[[1]]
  )

  # Split these into separate columns
  name_parts <- c(
    "genus_hybrid_sign",
    "genus_name",
    "species_hybrid_sign",
    "specific_epithet",
    "infraspecific_rank",
    "infraspecific_epithet",
    "author"
  )

  parsed_names <- tidyr::separate(
    data = parsed_names,
    col = record,
    into = c("id", name_parts),
    sep = "\\|",
    fill = "right",
    remove = FALSE
  )

  # Fill in NA if that name part is missing
  parsed_names[parsed_names == ""] <- NA

  # Add "fail" column if all name parts are missing (couldn't be parsed properly)
  parsed_names$fail <- sapply(
    1:nrow(parsed_names),
    function(x) all(is.na(parsed_names[x, name_parts]))
  )

  # Early exit if everything failed
  assertthat::assert_that(
    !all(parsed_names$fail == TRUE),
    msg = "No names could be successfully parsed"
  )

  # Emit warning for failures
  if (sum(parsed_names$fail) > 0 && quiet == FALSE) {
    failed_ids <- parsed_names$id[parsed_names$fail == TRUE]
    failed_names <- paste(
      taxa_tbl$name[taxa_tbl$id %in% failed_ids],
      collapse = ", "
    )
    warning(glue::glue(
      "The following names could not be parsed and are excluded from results: {failed_names}"
    ))
  }

  # Add back in original name
  parsed_names <- dplyr::left_join(
    parsed_names,
    dplyr::select(taxa_tbl, id, name),
    by = "id"
  )

  # Remove failures, drop "fail" column
  parsed_names <- parsed_names[parsed_names$fail == FALSE, ]
  parsed_names$fail <- NULL

  # Return parsed names as dataframe or tibble
  results <- parsed_names[, c("name", "id", name_parts)]

  if (isTRUE(tbl_out)) return(tibble::as_tibble(results))

  results
}
