
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxastand

<!-- badges: start -->

<!-- badges: end -->

**NOTE:** This package is under **active development**. Functions may
change drastically, and the user should not expect the interface to stay
consistent.

The goal of `taxastand` is to standardize species names from different
sources, a common task in biology. Very often different biologists use
different synonyms to refer to the same species. If we want to join data
from different sources, their taxonomic names must be standardized
first. This is what `taxastand` seeks to do in a reproducible and
efficient manner.

`taxastand` is based on matching names to a single **taxonomic
standard**, that is, a database of accepted names and synonyms. As long
as a single taxonomic standard is used, we can confidently resolve names
from disparate sources.

The taxonomic standard must conform to [Darwin Core
standards](https://dwc.tdwg.org/). The user must provide this database
(as a dataframe). There are many sources of taxonomic data online,
including
[GBIF](https://www.gbif.org/en/dataset/d7dddbf4-2cf0-4f39-9b2a-bb099caae36c),
[Catalog of Life](http://www.catalogueoflife.org/), and
[ITIS](https://www.itis.gov/) to name a few. The
[taxadb](https://cboettig.github.io/taxadb/index.html) package provides
convenient functions for downloading various taxonomic databases that
use Darwin Core.

## Installation

`taxastand` is currently only available on GitHub:

``` r
install.packages("devtools")

# Install another github-hosted dependency first
devtools::install_github("joelnitta/jntools")

devtools::install_github("joelnitta/taxastand")
```

## Similar work

[ROpenSci](https://ropensci.org/) has a [task
view](https://github.com/ropensci/taxonomy) summarizing many tools
available for taxonomy.

[taxize](https://github.com/ropensci/taxize) is the “granddaddy” of
taxonomy packages in R. It can search around 20 different taxonomic
databases for names and retrieve taxonomic information. It does not
store any databases locally.

[taxizedb](https://github.com/ropensci/taxizedb) downloads taxonomic
databases and provides tools to interface with them through SQL.

[taxadb](https://cboettig.github.io/taxadb/index.html) also downloads
and searches taxonomic databases. It can interface with them either
through SQL or in-memory in
R.

[taxonstand](https://cran.r-project.org/web/packages/Taxonstand/index.html)
has a very similar goal to `taxastand`, but only uses [The Plant
List](www.theplantlist.org) as its taxonomic standard and does not allow
the user to provide their own.

## Examples

Here are some examples that run quickly using the small example dataset
included with the package.

``` r
library(taxastand)

# Load reference taxonomy in Darwin Core format
data(filmy_taxonomy)

# This taxon matches many names at the species level because
# there are a bunch of varieties.
match_taxonomy("Hymenophyllum polyanthos", filmy_taxonomy, "species")
#> # A tibble: 12 x 13
#>    query n_hits distance match_to match_by taxonID acceptedNameUsa…
#>    <chr>  <dbl>    <dbl> <chr>    <chr>      <dbl>            <dbl>
#>  1 Hyme…     12        0 Hymenop… species   5.41e7         54115223
#>  2 Hyme…     12        0 Hymenop… species   5.41e7         54115393
#>  3 Hyme…     12        0 Hymenop… species   5.41e7         54115518
#>  4 Hyme…     12        0 Hymenop… species   5.41e7         54115518
#>  5 Hyme…     12        0 Hymenop… species   5.41e7         54115524
#>  6 Hyme…     12        0 Hymenop… species   5.41e7         54115533
#>  7 Hyme…     12        0 Hymenop… species   5.41e7         54115533
#>  8 Hyme…     12        0 Hymenop… species   5.41e7         54115574
#>  9 Hyme…     12        0 Hymenop… species   5.41e7         54115614
#> 10 Hyme…     12        0 Hymenop… species   5.41e7               NA
#> 11 Hyme…     12        0 Hymenop… species   5.41e7         54115620
#> 12 Hyme…     12        0 Hymenop… species   5.41e7         54115649
#> # … with 6 more variables: taxonomicStatus <chr>, taxonRank <chr>,
#> #   scientificName <chr>, genus <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>

# Using the full species name with author can get us a
# more exact match.
match_taxonomy("Hymenophyllum polyanthos (Sw.) Sw.", filmy_taxonomy, "scientific_name")
#> # A tibble: 1 x 13
#>   query n_hits distance match_to match_by taxonID acceptedNameUsa…
#>   <chr>  <dbl>    <dbl> <chr>    <chr>      <dbl>            <dbl>
#> 1 Hyme…      1        0 Hymenop… scienti…  5.41e7               NA
#> # … with 6 more variables: taxonomicStatus <chr>, taxonRank <chr>,
#> #   scientificName <chr>, genus <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>

# Fuzzy match helps when the query didn't abbreviate
# the author, but it is abbreviated in the reference.
match_taxonomy("Hymenophyllum polyanthos (Swartz) Swartz",
filmy_taxonomy, "scientific_name", max_dist = 8)
#> # A tibble: 1 x 13
#>   query n_hits distance match_to match_by taxonID acceptedNameUsa…
#>   <chr>  <dbl>    <dbl> <chr>    <chr>      <dbl>            <dbl>
#> 1 Hyme…      1        8 Hymenop… scienti…  5.41e7               NA
#> # … with 6 more variables: taxonomicStatus <chr>, taxonRank <chr>,
#> #   scientificName <chr>, genus <chr>, specificEpithet <chr>,
#> #   infraspecificEpithet <chr>
```
