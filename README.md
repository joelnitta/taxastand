
<!-- README.md is generated from README.Rmd. Please edit that file -->

# taxastand <img src="man/figures/logo.png" align="right" alt="" width="120" />

<!-- badges: start -->

[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![DOI](https://zenodo.org/badge/192684959.svg)](https://zenodo.org/badge/latestdoi/192684959)
[![R-CMD-check](https://github.com/joelnitta/taxastand/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/joelnitta/taxastand/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/joelnitta/taxastand/graph/badge.svg)](https://app.codecov.io/gh/joelnitta/taxastand)
<!-- badges: end -->

The goal of `taxastand` is to standardize species names from different
sources, a common task in biology.

Very often different biologists use different synonyms to refer to the
same species. If we want to join data from different sources, their
taxonomic names must be standardized first. This is what `taxastand`
seeks to do in a reproducible and efficient manner.

`taxastand` parses each name into its parts (genus, epithet,
infraspecific rank, author and so on), matches them to a reference
taxonomy that you choose while allowing for spelling differences in the
epithet and author, and resolves synonyms to their accepted names.

## Learn more

- [The
  basics](https://joelnitta.github.io/taxastand/articles/basics.html):
  the parse, match and resolve workflow
- [Setting up a
  reference](https://joelnitta.github.io/taxastand/articles/setup-reference.html):
  how to choose and prepare a reference for matching
- [Benchmarks](https://joelnitta.github.io/taxastand/articles/benchmark.html):
  performance comparison with similar packages
- [Function reference](https://joelnitta.github.io/taxastand/reference/)

## Installation

`taxastand` can be installed from
[r-universe](https://joelnitta.r-universe.dev) or
[github](https://github.com/joelnitta).

``` r
install.packages("taxastand", repos = 'https://joelnitta.r-universe.dev')
```

OR

``` r
# install.packages("remotes")
remotes::install_github("joelnitta/taxastand")
```

Some of the sources of `ts_load_ref()` are Parquet files, which need the
[nanoparquet](https://nanoparquet.r-lib.org/) or
[duckdb](https://r.duckdb.org/) package. Neither is installed with
`taxastand`; it will let you know if one is needed.

## Taxonomic standard

`taxastand` is based on matching names to a single **taxonomic
standard**, that is, a database of accepted names and synonyms. As long
as a single taxonomic standard is used, we can confidently resolve names
from disparate sources.

The taxonomic standard must conform to [Darwin Core
standards](https://dwc.tdwg.org/) and is supplied by the user as a
dataframe (if your dataframe doesn’t use the exact same column names as
Darwin Core, `taxastand` is flexible). `ts_load_ref()` downloads and
formats several widely used reference databases, including [World Flora
Online](https://www.worldfloraonline.org/), the [Pteridophyte Phylogeny
Group](https://github.com/pteridogroup/ppg) classification,
[ITIS](https://www.itis.gov/), [Catalogue of
Life](https://www.catalogueoflife.org/) and the
[GBIF](https://www.gbif.org/) backbone. It can also read your own file
or URL, and `ts_subset_ref()` keeps just one family or other group of a
large source. See `vignette("setup-reference")` for details, including
how to use tables from other packages such as
[taxadb](https://github.com/ropensci/taxadb).

``` r
library(taxastand)

ts_show_ref_sources()
ref <- ts_load_ref(source = "ppg")
```

Downloads happen only when `ts_load_ref()` is called, and go to a
temporary folder unless you set `cache_dir`.

## Example

Here is an example of fuzzy matching followed by resolution of synonyms
using the dataset included with the package.

``` r
library(taxastand)

# Load example reference taxonomy in Darwin Core format
data(filmy_taxonomy)

# Take a look at the columns used by taxastand
head(filmy_taxonomy[c(
  "taxonID", "acceptedNameUsageID", "taxonomicStatus", "scientificName"
)])
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

# Names to resolve: an exact match, a match that differs in the author's
# abbreviation, and an outdated name with a misspelled epithet
queries <- c(
  "Hymenophyllum wilsonii Hook.",
  "Crepidomanes minutum (Blume) K.Iwats.",
  "Gonocormus minutum"
)

res <- ts_resolve_names(queries, filmy_taxonomy)
res[c("query", "resolved_name", "matched_status", "match_type")]
#>                                   query                        resolved_name
#> 1          Hymenophyllum wilsonii Hook.         Hymenophyllum wilsonii Hook.
#> 2 Crepidomanes minutum (Blume) K.Iwats. Crepidomanes minutum (Bl.) K. Iwats.
#> 3                    Gonocormus minutum Crepidomanes minutum (Bl.) K. Iwats.
#>   matched_status       match_type
#> 1  accepted name            exact
#> 2  accepted name     auto_basexin
#> 3        synonym auto_fuzzy_canon
```

`resolved_name` is the accepted name for each query, and `match_type`
says how it was found (`exact`, or one of the `auto_*` types for matches
that needed some tolerance; see `?ts_match_names`). The `resolved_name`
column can be used for downstream analyses joining on other datasets
that have been resolved to the same reference taxonomy.

## Similar work

- [taxon-tools](https://github.com/camwebb/taxon-tools) is a
  command-line interface program that was the inspiration for
  `taxastand`. `taxastand` originally required it as a dependency
  (removed in v2.0) and was a faithful port of its algorithms, but
  `taxastand` is now developed independently.

- [taxize](https://github.com/ropensci/taxize) is the “granddaddy” of
  taxonomy packages in R. It can search around 20 different taxonomic
  databases for names and retrieve taxonomic information.

- [TNRS](https://tnrs.biendata.org/), the Taxonomic Name Resolution
  Service, is a web application that resolves taxonomic names of plants
  against several databases.

- [taxizedb](https://github.com/ropensci/taxizedb) downloads taxonomic
  databases and provides tools to interface with them through SQL.

- [taxadb](https://github.com/ropensci/taxadb) also downloads and
  searches taxonomic databases. It can interface with them either
  through SQL or in-memory in R.

- [taxonstand](https://CRAN.R-project.org/package=Taxonstand) has a very
  similar goal to `taxastand`, but only uses [The Plant List
  (TPL)](https://web.archive.org/web/20260619054601/http://www.theplantlist.org/)
  as its taxonomic standard and does not allow the user to provide their
  own. Note that TPL is no longer being updated as of 2013, and its
  website is no longer online (linked above via the Wayback Machine).

- [taxify](https://github.com/gcol33/taxify),
  [WorldFlora](https://CRAN.R-project.org/package=WorldFlora) and
  [U.Taxonstand](https://github.com/ecoinfor/U.Taxonstand) also match
  names in R against a reference taxonomy. The
  [Benchmarks](https://joelnitta.github.io/taxastand/articles/benchmark.html)
  article compares them with `taxastand`.

- [rOpenSci](https://ropensci.org/) has a [task
  view](https://github.com/ropensci/taxonomy) summarizing many tools
  available for taxonomy.

## Motivation

Although existing web-based solutions for taxonomic name resolution are
very useful, they may not be ideal for all situations: the choice of
reference database to use for standardization is limited, they may not
be able to handle very large queries, and the user has no guarantee that
the same input will yield the same output at a later date due to changes
in the remote database.

Furthermore, matching of taxonomic names is not straightforward, since
they are complex data structures including multiple components (e.g.,
genus, specific epithet, basionym author, combination author, etc).
Several of the [tools mentioned above](#similar-work) match names
fuzzily, but they differ in whether they take the author into account
and whether the user can choose and version the reference database; the
[Benchmarks](https://joelnitta.github.io/taxastand/articles/benchmark.html)
article compares them.

The motivation for `taxastand` is to provide greater flexibility and
reproducibility by allowing for complete version control of the code and
database used for name resolution, while implementing fuzzy matching of
parsed taxonomic names.

## Contributing

Contributions are welcome! Please see the [contributing
guide](.github/CONTRIBUTING.md) for how to file issues, submit pull
requests, and run checks locally.

Please note that this package is released with a [Contributor Code of
Conduct](https://ropensci.org/code-of-conduct/). By contributing to this
project, you agree to abide by its terms.

## Citing this package

If you use this package, please cite it. `citation("taxastand")` gives
the reference, for example:

    Nitta, JH (2026) taxastand: Taxonomic Name Standardization. https://doi.org/10.5281/zenodo.5726390

The example DOI above is for the overall package.

Please also cite the reference taxonomy you matched against. For sources
loaded with `ts_load_ref()`, see `ts_show_ref_sources()` or the
`ts_source` attribute of the result.

Here is the latest DOI, which you should use if you are using the latest
version of the package:

[![DOI](https://zenodo.org/badge/192684959.svg)](https://zenodo.org/badge/latestdoi/192684959)

You can find DOIs for older versions by viewing the “Releases” menu on
the right.
