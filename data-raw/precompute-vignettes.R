# Precompute vignettes that need packages or network access that taxastand does
# not depend on (the method of
# https://ropensci.org/blog/2019/12/08/precompute-vignettes/).
#
# Each vignettes/<name>.Rmd.orig is knit to vignettes/<name>.Rmd, with the code
# output included. Edit the .orig file, never the generated .Rmd, then run:
#
#   Rscript data-raw/precompute-vignettes.R
#
# taxadb.Rmd.orig needs the taxadb and dplyr packages and a network connection.

old <- setwd("vignettes")
knitr::knit("taxadb.Rmd.orig", output = "taxadb.Rmd")
setwd(old)
