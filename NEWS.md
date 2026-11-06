# taxastand 2.0.0.9000

## Major changes

* Design philosophy change: taxastand is no longer a strict port of
  taxon-tools. Goals are now to standardize names accurately (including
  authors), quickly, and flexibly (custom reference databases), rather than
  to implement taxon-tools in R. Output may diverge from taxon-tools in
  future releases. The docker differential test and the "pure R vs.
  taxon-tools" benchmark vignette were removed.

* Add `strip_qualifiers` argument to `ts_parse_names()`, `ts_match_names()`,
  and `ts_resolve_names()`, so that open-nomenclature qualifiers (`cf.`,
  `aff.`, `s.l.`/`sensu lato`, `s.str.`/`sensu stricto`) can optionally be
  removed from names before parsing/matching, instead of being mis-captured
  as the specific epithet. Off by default (#40)

## Performance

* Name matching is much faster for large references (e.g. ~20 s to ~1 s for
  5 queries against the 78k-name PPG taxonomy). `ts_match_names()` and
  `ts_resolve_names()` now ignore reference names whose genus cannot match any
  query genus before parsing the reference (#43); match keys are computed
  lazily and looked up in vectorised passes instead of per-key hash maps
  (#45); and `tt_matchnames()` returns a data frame instead of delimited
  strings (#46). Results are unchanged, except that multiple fuzzy matches to
  one query are now returned in reference order.

## Bug fixes

* Fix `ts_match_names(..., collapse_infra = TRUE)`'s internal deduplication
  key, which used `paste0(..., sep = "_")` -- `paste0()` has no `sep`
  argument, so the separator was silently dropped, joining name components
  with no delimiter between them. This could in rare cases cause two
  genuinely different names to be treated as duplicates during
  deduplication if their concatenated components happened to coincide.
  Found via static analysis while running `pkgcheck::pkgcheck()` ahead of
  rOpenSci submission.

## Minor improvements

* Use `vapply()` instead of `sapply()`, `anyDuplicated()` instead of
  `any(duplicated(x))`, and drop several redundant `x == TRUE`/`x == FALSE`
  comparisons
* Fix a dead link in the README ("The Plant List", no longer online; now
  links to a Wayback Machine snapshot)

# taxastand 2.0.0

## Major changes

* Implement pure R solution for name parsing and matching
* Add `manual_match` argument to `ts_match_names()`, so users can supply a
  dataframe of manually matched names that override the matching algorithm
  (#4)

# taxastand 1.0.0

## Major changes

* Add Docker support for running `taxon-tools`, so it no longer needs to be
  installed locally
* Add `collapse_infra` and `collapse_infra_exclude` arguments to
  `ts_match_names()` and `ts_resolve_names()`, for collapsing infraspecific
  taxa to their species when matching
* Add `quiet` argument to `ts_parse_names()` to suppress warnings

## Bug fixes

* Fix mapping of match results after collapsing infraspecific taxa
* Fix `matched_status` incorrectly showing as `NA` for some results
* Replace empty strings with `NA` in results

# taxastand 0.0.0.9000

## Major changes

* Initial implementation of `ts_parse_names()`, `ts_match_names()`, and
  `ts_resolve_names()`, using the external `taxon-tools` system dependency
  for parsing and fuzzy matching
* Add package website via pkgdown and an introductory vignette
