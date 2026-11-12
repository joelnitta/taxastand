# Precompute vignettes that need packages or network access that taxastand does
# not depend on (the method of
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/).
#
# Each vignettes/<name>.Rmd.orig is knit to vignettes/<name>.Rmd, with the code
# output included. Edit the .orig file, never the generated .Rmd, then run:
#
#   Rscript data-raw/precompute-vignettes.R
#
# setup-reference.Rmd.orig needs the taxadb, dplyr and nanoparquet packages and a network connection.

withr::with_dir(
  "vignettes",
  knitr::knit("setup-reference.Rmd.orig", output = "setup-reference.Rmd")
)
