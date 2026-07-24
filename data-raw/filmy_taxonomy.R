library(tidyverse)

# Load the example standard taxonomy for resolving names.
#
# The example standard taxonomy is the family Hymenophyllaceae from
# Catalog of Life (CoL). It was originally obtained by selecting
# "Hymenophyllaceae" for "family" and "Complete data" on
# http://www.catalogueoflife.org/DCA_Export/index.php on 2019-06-19, by
# downloading:
# http://www.catalogueoflife.org/DCA_Export/zip/archive-family-hymenophyllaceae-bl3.zip
#
# That URL no longer resolves (confirmed 404, checked 2026-07-24): the
# legacy "DCA_Export" endpoint has been retired along with the old
# catalogueoflife.org site. Since the exact original export can no longer
# be re-downloaded, the data as originally processed (species rank and
# below, with the author name fix described below already applied) is
# archived at data-raw/taxa_hymenophyllaceae_col_2019-06-19.tsv so this
# script remains reproducible without depending on a live external URL.
taxa_path <- "data-raw/taxa_hymenophyllaceae_col_2019-06-19.tsv"

filmy_taxonomy <- read_tsv(taxa_path)

# Replace "v. d. Bosch" with "V. D. Bosch"
# see https://github.com/camwebb/taxon-tools/issues/10
filmy_taxonomy <-
  filmy_taxonomy %>%
  dplyr::mutate(
    scientificName = stringr::str_replace_all(
      scientificName,
      "v. d. Bosch",
      "V. D. Bosch"
    )
  )

usethis::use_data(filmy_taxonomy, overwrite = TRUE)
