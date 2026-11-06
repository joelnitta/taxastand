# taxastand 2.0.0.9000

## Major changes

* **Breaking: placeholder names are excluded (#41).** Names such as
  `"Genus sp."`, `"Genus spp."`, `"Genus indet."`, `"Genus stet."` or
  `"Genus sp. inc."` (an identification that stopped above species rank) used
  to be parsed with `"sp"` as the specific epithet and could be fuzzy-matched
  to unrelated species. They are now dropped by `ts_parse_names()`,
  `ts_match_names()` and `ts_resolve_names()` with a warning. Use the new
  argument `exclude_placeholders = FALSE` for the old behaviour.
* **Breaking: fuzzy matching of queries without an author (#49).** The author
  string used to count towards the edit distance, so author-less queries could
  only fuzzy-match (loosely, and only because the old default `max_dist` was
  large). They are now fuzzy-matched on the canonical name (genus + specific
  epithet + infraspecific epithet), ignoring the author. A canonical-name match
  is reported as `auto_cfonly`, and a misspelled one as the new match type
  `auto_fuzzy_canon`; all reference names within `max_dist` are returned, so
  ambiguity shows up in `ts_resolve_names()`. The new argument
  `fuzzy_canon_hits = "closest"` instead keeps only the nearest hits (ties kept),
  which avoids unrelated names with short epithets. Queries with an author are
  still matched on the whole name (`auto_fuzzy`).

* **Breaking: the default `max_dist` is now 3 (was 10) (#47).** `max_dist` is
  the Levenshtein distance on the whole depunctuated name including the author,
  and 10 was very loose (for example, 5 queries against PPG returned 275 result
  rows). The distance is also now plain Levenshtein distance: the extra +1
  penalty taxon-tools applied when the reference merely has extra trailing text
  was removed, which can add matches at exactly `max_dist`.

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
  strings (#46). `ts_parse_names()` is also about twice as fast, because the
  per-row failure check and `tidyr::separate()` were replaced with vectorised
  base R (#44). Results are unchanged, except that multiple fuzzy matches to
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

* Document the `match_type` values in a new "Match types" section of
  `?ts_match_names`, grouped by how far each can be trusted (#52); the match
  type codes themselves are unchanged
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
