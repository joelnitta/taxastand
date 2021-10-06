#' Parse taxonomic names with (and for) taxon-tools
#' 
#' Requires [taxon-tools](https://github.com/camwebb/taxon-tools) to be installed.
#' 
#' Parses scientific names into their component parts (genus, species, variety, author, etc).
#'
#' @param taxa Character vector; taxon names to be parsed by taxon-tools `parsenames`.
#' Missing values not allowed. Must all be unique.
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
#' ts_parse("Foogenus x barspecies var. foosubsp (L.) F. Bar")
#'
ts_parse <- function(taxa) {
  
  # Check input: must be character vector, no NA values, all unique
  assertthat::assert_that(is.character(taxa))
  assertthat::assert_that(assertthat::noNA(taxa), msg = "Input taxa may not contain NAs")
  assertthat::assert_that(all(assertr::is_uniq(taxa)), msg = "Input taxa must be unique")
  
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
  if(fs::file_exists(ref_taxa_txt_file)) fs::file_delete(ref_taxa_txt_file)
  writeLines(taxa_tbl$record, ref_taxa_txt_file)
  
  # Parse reference names with taxon tools
  ref_parsed <- processx::run("parsenames", ref_taxa_txt_file)
  if(fs::file_exists(ref_taxa_txt_file)) fs::file_delete(ref_taxa_txt_file)
  
  # Read in results of parsing, format as dataframe
  
  # The output is originally one record per line, with fields separated by '|' (pipe symbol)
  parsed_res <- data.frame(record = strsplit(ref_parsed[["stdout"]], "\n")[[1]])
  
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
  
  parsed_res <- tidyr::separate(
    data = parsed_res, 
    col = record, 
    into = c("id", name_parts), 
    sep = "\\|", 
    fill = "right",
    remove = FALSE)
  
  # Fill in NA if that name part is missing
  parsed_res[parsed_res == ""] <- NA
  
  # Add "fail" column if all name parts are missing (couldn't be parsed properly)
  parsed_res$fail <- sapply(1:nrow(parsed_res), function(x) all(is.na(parsed_res[x, name_parts])))
  
  # Early exit if everything failed
  assertthat::assert_that(
    !all(parsed_res$fail == TRUE),
    msg = "No names could be successfully parsed")
  
  # Emit warning for failures
  if(sum(parsed_res$fail) > 0) {
    failed_ids <- parsed_res$id[parsed_res$fail == TRUE]
    failed_names <- paste(taxa_tbl$name[taxa_tbl$id %in% failed_ids], collapse = ", ") 
    warning(glue::glue("The following names could not be parsed and are excluded from results: {failed_names}"))
  }
  
  # Add back in original name
  parsed_res <- dplyr::left_join(
    parsed_res,
    dplyr::select(taxa_tbl, id, name), by = "id")
  
  # Remove failures, drop "fail" column
  parsed_res <- parsed_res[parsed_res$fail == FALSE, ]
  parsed_res$fail <- NULL
  
  # Return parsed names as dataframe
  parsed_res[, c("name", "id", name_parts)]
}