library(tidyverse)

# Load the example standard taxonomy for resolving names.

# The example standard taxonomy is the family Hymenophyllaceae from
# Catalog of Life (CoL). CoL provides persistant links to database dumps.
# This one was obtained by selecting "Hymenophyllaceae" for "family"
# and "Complete data" on http://www.catalogueoflife.org/DCA_Export/index.php
# on 2019-06-19

# Download the zip file
temp_dir <- fs::dir_create(tempdir())
download.file(
  "http://www.catalogueoflife.org/DCA_Export/zip/archive-family-hymenophyllaceae-bl3.zip",
  fs::path(temp_dir, "archive-genus-vandenboschia-bl3.zip")
)

# Unzip
unzip(
  fs::path(temp_dir, "archive-genus-vandenboschia-bl3.zip"),
  exdir = temp_dir)

# Read in taxonomy table, keep only
# names at species rank and below
# (warnings are produced because names at genus level
# and above have NA for many fields).
filmy_taxonomy <- read_tsv(fs::path(temp_dir, "taxa.txt")) %>%
  filter(str_detect(taxonRank, "species"))

# Replace "v. d. Bosch" with "V. D. Bosch"
# see https://github.com/camwebb/taxon-tools/issues/10
filmy_taxonomy <-
filmy_taxonomy %>%
  dplyr::mutate(scientificName = stringr::str_replace_all(scientificName, "v. d. Bosch", "V. D. Bosch"))

usethis::use_data(filmy_taxonomy)
