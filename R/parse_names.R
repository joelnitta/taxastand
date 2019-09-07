#' Add species and taxon names to a dataframe by
#' parsing the scientific names with gnparser
#'
#' Requires gnparser (https://gitlab.com/gogna/gnparser/)
#' to be installed and on $PATH
#'
#' @param df Dataframe
#' @param sci_name Unquoted name of column in dataframe with
#' scientific name to be parsed
#' @param parsed_species_name Unquoted name of column to add with
#' species name
#' @param parsed_taxon_name  Unquoted name of column to add with
#' taxon name
#'
#' @return Tibble
#' @examples
#' names_df <- tibble::tibble(name = c(
#'   "Amaurorhinus bewichianus (Wollaston,1860) (s.str.)",
#'   "Crepidomanes minutum var. minutum"))
#' add_parsed_names(names_df, name, species, taxon)
#' @export
add_parsed_names <- function (df, sci_name, parsed_species_name, parsed_taxon_name) {

  sci_name_enq <- rlang::enquo(sci_name)
  parsed_species_name <- rlang::enquo(parsed_species_name)
  parsed_taxon_name <- rlang::enquo(parsed_taxon_name)

  parsed_names <-
    df %>%
    dplyr::pull(!!sci_name_enq) %>%
    unique %>%
    parse_names_batch() %>%
    dplyr::select(!!sci_name_enq := b, !!parsed_taxon_name := c) %>%
    dplyr::mutate(!!parsed_species_name := sp_name_only(!!parsed_taxon_name))

  suppressMessages(dplyr::left_join(df, parsed_names))
}

#' Parse species names in batch
#'
#' Runs much faster for a large number of names.
#'
#' Requires gnparser to be installed and on $PATH
#'
#' @param names Character vector of species names.
#' May include author, variety, etc. All names must be
#' unique, with no NAs.
#' @param check Logical; should a check be made that the
#' results original name match the names of the input?
#'
#' @return Tibble
#'
#' @examples
#' parse_names_batch("Amaurorhinus bewichianus (Wollaston,1860) (s.str.)", gnparser_path = "/Users/joel/Downloads/gnparser")
#' @export
parse_names_batch <- function (names, check = TRUE, gnparser_path = NULL) {

  assertthat::assert_that(is.character(names))
  assertthat::assert_that(all(assertr::not_na(names)))
  assertthat::assert_that(all(assertr::is_uniq(names)))

  temp_file <- fs::file_temp() %>% fs::path_ext_set("txt")

  temp_dir <- fs::path_dir(temp_file)

  temp_txt <- fs::path_file(temp_file)

  readr::write_lines(names, temp_file)

  args = c(
    "-f",
    "simple",
    "-j",
    "20",
    temp_txt
  )
  
  # Specify path to gnparser, if given
  gnparser_command <- "gnparser"
  
  if(!is.null(gnparser_path)) {
    assertthat::assert_that(fs::file_exists(gnparser_path))
    gnparser_command <- fs::path_abs(gnparser_path)
  }

  results <-
    processx::run(
      command = gnparser_command, args, wd = temp_dir) %>%
    magrittr::extract("stdout") %>%
    unlist() %>%
    readr::read_lines() %>%
    stringr::str_split("\\|") %>%
    # Need to figure out what each column actually means
    purrr::map(~purrr::set_names(., letters[1:7])) %>%
    do.call(dplyr::bind_rows, .) %>%
    dplyr::select(b:g)

  if (isTRUE(check)) {
    # b contains the original name. Sometimes these can get out of order.
    # Make sure they are in the same order as the input.
    results <-
      dplyr::arrange(results, match(b,names)) %>%
      assertr::verify(all(.$b == names))
  }

  results

}
