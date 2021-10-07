
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxastand

<!-- badges: start -->

[![Project Status: WIP – Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
<!-- badges: end -->

The goal of `taxastand` is to standardize species names from different
sources, a common task in biology. Very often different biologists use
different synonyms to refer to the same species. If we want to join data
from different sources, their taxonomic names must be standardized
first. This is what `taxastand` seeks to do in a reproducible and
efficient manner.

## Important note

**This package is in early development.** There may be major, breaking
changes to functionality in the near future. If you use this package, I
highly recommend using a package manager like
[renv](https://rstudio.github.io/renv/articles/renv.html) so that later
updates won’t break your code.

## Taxonomic standard

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
[taxadb](https://github.com/ropensci/taxadb) package provides convenient
functions for downloading various taxonomic databases that use Darwin
Core.

## Installation

`taxastand` is currently only available on GitHub:

``` r
# install.packages("remotes")

remotes::install_github("joelnitta/taxastand")
```

## Dependencies

`taxastand` depends on
[taxon-tools](https://github.com/camwebb/taxon-tools) for taxonomic name
matching. The two programs included in `taxon-tools`, `parsenames` and
`matchnames`, must be installed and on the user’s `PATH`.

## Similar work

[ROpenSci](https://ropensci.org/) has a [task
view](https://github.com/ropensci/taxonomy) summarizing many tools
available for taxonomy.

[taxize](https://github.com/ropensci/taxize) is the “granddaddy” of
taxonomy packages in R. It can search around 20 different taxonomic
databases for names and retrieve taxonomic information.

[TNRS](http://tnrs.iplantcollaborative.org/), the Taxonomic Name
Resolution Service, is a web application that resolves taxonomic names
of plants according to one of six databases.

[taxizedb](https://github.com/ropensci/taxizedb) downloads taxonomic
databases and provides tools to interface with them through SQL.

[taxadb](https://github.com/ropensci/taxadb) also downloads and searches
taxonomic databases. It can interface with them either through SQL or
in-memory in R.

[taxonstand](https://cran.r-project.org/web/packages/Taxonstand/index.html)
has a very similar goal to `taxastand`, but only uses [The Plant List
(TPL)](http://www.theplantlist.org) as its taxonomic standard and does
not allow the user to provide their own. Note that TPL is no longer
being updated as of 2013.

## Motivation

Although existing web-based solutions for taxonomic name resolution are
very useful, they may not be ideal for all situations: the choice of
reference database to use for standardization is limited, they may not
be able to handle very large queries, and the user has no guarantee that
the same input will yield the same output at a later date due to changes
in the remote database.

Furthermore, matching of taxonomic names is not straightforward, since
they are complex data structures including multiple components (e.g.,
genus, specific epithet, basionym author, combination author, etc). [Of
the tools mentioned above](#similar-work) only
[TNRS](http://tnrs.iplantcollaborative.org/) can fuzzily match taxonomic
names based on their parsed components, but it does not allow for use of
a local reference database.

The motivation for `taxastand` is to provide greater flexibility and
reproducibility by allowing for complete version control of the code and
database used for name resolution, while implementing fuzzy matching of
parsed taxonomic names.

## Example

Here is an example of fuzzy matching followed by resolution of synonyms
using the dataset included with the package.

``` r
library(taxastand)

# Load example reference taxonomy in Darwin Core format
data(filmy_taxonomy)

# Take a look at the columns used by taxastand
head(filmy_taxonomy[c("taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName")])
#>    taxonID acceptedNameUsageID taxonomicStatus
#> 1 54115096                  NA   accepted name
#> 2 54133783            54115097         synonym
#> 3 54115097                  NA   accepted name
#> 4 54133784            54115098         synonym
#> 5 54115098                  NA   accepted name
#> 6 54133785            54115099         synonym
#>                              scientificName
#> 1             Cephalomanes atrovirens Presl
#> 2                Trichomanes crassum Copel.
#> 3 Cephalomanes crassum (Copel.) M. G. Price
#> 4           Trichomanes densinervium Copel.
#> 5 Cephalomanes densinervium (Copel.) Copel.
#> 6         Trichomanes infundibulare Alderw.

# As a test, resolve a misspelled name
ts_resolve_names("Gonocormus minutum", filmy_taxonomy)
#>                query                        accepted_name match_type  status
#> 1 Gonocormus minutum Crepidomanes minutum (Bl.) K. Iwats. auto_fuzzy synonym
#>                        reference
#> 1 Gonocormus minutus (Bl.) Bosch

# We can now use the `accepted_name` column of this result for downstream 
# analyses joining on other datasets that have been resolved to the same 
# reference taxonomy.
```
