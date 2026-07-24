# Contributing to taxastand

This outlines how to propose a change to taxastand.

By participating in this project you agree to abide by its
[Contributor Code of Conduct](https://ropensci.org/code-of-conduct/).

## Scope

taxastand matches and standardizes taxonomic names against a
user-supplied reference taxonomy (see the README for details). Feature
requests that are really about a specific taxonomic database or web
service are likely a better fit for packages like
[taxize](https://github.com/ropensci/taxize) or
[taxadb](https://github.com/ropensci/taxadb); if you're not sure, open
an issue to discuss before starting work.

## Filing an issue

If you've found a bug, please file an issue that illustrates it with a
minimal reproducible example. If you'd like to propose a new feature,
open an issue describing what you'd like to see and why, before writing
code — this saves everyone time if it turns out the feature is out of
scope or needs a different approach.

## Pull requests

To contribute a change:

1. Fork the repo and create a new branch from `main`.
2. Make your change.
3. If you've changed code, add or update tests as needed.
4. Update documentation if you've changed function behavior or arguments
   (run `devtools::document()` to regenerate `.Rd` files after editing
   roxygen comments).
5. Make sure the package passes checks locally before opening a PR (see
   below).
6. Push your branch and open a pull request describing the change.

For small fixes (typos, obvious bugs), feel free to skip the issue and go
straight to a pull request.

## Running checks locally

```r
# Install development dependencies
devtools::install_dev_deps()

# Run the test suite
devtools::test()

# Check code style (uses the .lintr config in this repo)
lintr::lint_package()

# Auto-format code to match the project's style
styler::style_pkg()

# Full package check (mirrors CI)
devtools::check()
```

Some tests compare taxastand's pure-R matching engine against the
original [taxon-tools](https://github.com/camwebb/taxon-tools) Docker
image; these are skipped automatically if Docker isn't available, so
you don't need Docker to contribute.

## Code style

Code is formatted with [styler](https://styler.r-lib.org/) and linted
with [lintr](https://lintr.r-lib.org/) (see `.lintr` for the active
rules). Please run both before opening a PR.

## Documentation infrastructure

Function documentation is written as [roxygen2](https://roxygen2.r-lib.org/)
comments above each function in `R/`, not edited directly in `man/`.
Run `devtools::document()` to regenerate `.Rd` files and `NAMESPACE`
after changing roxygen comments.

## Package lifecycle

taxastand is under active development. Its public API
(`ts_parse_names()`, `ts_match_names()`, `ts_resolve_names()`) is
considered stable; internal functions may change without notice.

## Acknowledging contributions

Contributors who make a substantial code contribution will be credited
in `DESCRIPTION` (`Authors@R`, role `ctb`) and `NEWS.md`.
