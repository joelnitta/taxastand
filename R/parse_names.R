#' Add species and taxon names to a dataframe by
#' parsing the scientific names with gnparser
#'
#' Requires gnparser (https://gitlab.com/gogna/gnparser/)
#' to be installed.
#'
#' @param df Dataframe
#' @param sci_name Unquoted name of column in dataframe with
#' scientific name to be parsed
#' @param gnparser_path String; path to gnparser executable.
#' Either this must be provided, or gnparser must be on $PATH.
#' @param ... Unquoted taxonomic names to add. May include:
#' species, taxon, taxon_with_rank, author, or year.
#'
#' @return Tibble
#' @examples
#' names_df <- tibble::tibble(name = c(
#'   "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#'   "Crepidomanes minutum var. minutum"))
#' add_parsed_names(names_df, name, species, taxon)
#' add_parsed_names(names_df, name, species, author)
#' @export
add_parsed_names <- function (df, sci_name, ..., gnparser_path = NULL) {

  sci_name_enq <- rlang::enquo(sci_name)

  parsed_names <-
    df %>%
    dplyr::pull(!!sci_name_enq) %>%
    unique %>%
    parse_names_batch(gnparser_path = gnparser_path) %>%
    dplyr::select(!!sci_name_enq := query, ...)

  suppressMessages(dplyr::left_join(df, parsed_names))
}

#' Parse species names in batch
#'
#' Runs much faster for a large number of names.
#'
#' Requires gnparser (https://gitlab.com/gogna/gnparser/)
#' to be installed.
#'
#' @param names Character vector of species names.
#' May include author, variety, etc. All names must be
#' unique, with no NAs.
#' @param gnparser_path String; path to gnparser executable.
#' Either this must be provided, or gnparser must be on $PATH.
#'
#' @return Tibble
#'
#' @examples
#' parse_names_batch("Amaurorhinus bewichianus (Wollaston,1860) (s.str.)")
#' parse_names_batch(c(
#' "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#' "bla",
#' "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)")
#' )
#' @export
parse_names_batch <- function (names, gnparser_path = NULL) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(all(assertr::not_na(names)))

  # GNparser will do weird things (order of names gets scrambled)
  # if there are duplicate names. Run only on unique names,
  # then join back together to query at the end.
  names_table <- tibble::tibble(
    query = names
  )

  names <- unique(names)

  # Write out temp file for running GNparser outside of R
  temp_file <- fs::file_temp() %>% fs::path_ext_set("txt")

  temp_dir <- fs::path_dir(temp_file)

  temp_txt <- fs::path_file(temp_file)

  readr::write_lines(names, temp_file)

  # Set arguments for GNparser:
  args = c(
    "-f",
    "simple", # Simple output format
    "-j",
    "20",     # Max number of jobs to run concurrently
    temp_txt  # Names to parse
  )

  # Specify path to gnparser, if given
  gnparser_command <- "gnparser"

  if(!is.null(gnparser_path)) {
    assertthat::assert_that(fs::file_exists(gnparser_path))
    gnparser_command <- fs::path_abs(gnparser_path)
  }

  # Run GNparser and tidy the output
  results <-
    processx::run(
      command = gnparser_command, args, wd = temp_dir) %>%
    magrittr::extract("stdout") %>%
    unlist() %>%
    readr::read_lines() %>%
    stringr::str_split("\\|") %>%
    purrr::map(~purrr::set_names(., letters[1:7])) %>%
    do.call(dplyr::bind_rows, .) %>%
    # Select only relevant output columns
    dplyr::select(
      query = b,
      taxon = c,
      taxon_with_rank = d,
      author = e,
      year = f) %>%
    dplyr::mutate(species = sp_name_only(taxon))

  # Join back in to original query names.
  dplyr::left_join(names_table, results, by = "query")

}
