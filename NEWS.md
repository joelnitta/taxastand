# taxastand 2.0.0.9000

## Major changes

* **New `ts_optimize_ref()` for repeated matching against a large
  reference (#44, #110).** (Named `ts_optimize_ref()` to match the other reference helpers planned in #109.) Matching spends most of its time preparing the reference
  (finding the genus of every name, parsing, building match keys). Pass the
  result of `ts_optimize_ref(reference)` to `ts_match_names()` (as
  `reference`) or `ts_resolve_names()` (as `ref_taxonomy`) and that work is
  done once and reused: the genus of every name is found when the reference is
  prepared, and each genus is parsed and keyed the first time a query needs
  it. Results are identical. Against a 1.6M-name WFO reference, 100 queries
  take about 0.2 s and 1000 queries about 1.2 s with a prepared reference once
  the genera are cached, versus about 4 s and 11 s without; the first call that
  uses a genus pays for parsing it, as before. A prepared reference holds what
  it has built, so it grows with use.
* Fuzzy matching is faster for large references, with identical results (#44).
  Reference names whose length differs from the query's by more than `max_dist`
  can never be within `max_dist` edits, so they are no longer compared; the
  per-query bookkeeping of the matching stages was also made linear in the
  number of queries, and exact-key lookups no longer hash the whole reference.
* **Breaking: a query can now match several reference records at any stage
  (#105).** When several reference records reduce to the same name at the
  exact, punctuation, basionym, *in*, *ex*, rank or spelling-variant stages
  (for example "Willd. ex Less." and "Willd. ex DC." for the query "Willd."),
  `ts_match_names()` returned only one of them, chosen by reference order. It now
  returns every one, as it already did for canonical-name and fuzzy matches, so
  the ambiguity is visible. Queries that matched a single record are unchanged.
* **Breaking: with `match_autonyms = TRUE`, an autonym that matches exactly
  after being collapsed now has `match_type` `autonym` instead of `exact`
  (#95).** For example, the query "Blechnum lunare var. lunare" matching the
  reference name "Blechnum lunare" was reported as `exact` although the two
  names differ. Plain species names that match exactly are still `exact`, and a
  collapsed name that matches by another route keeps that match type.
* The `collapse_infra` and `collapse_infra_exclude` arguments of
  `ts_match_names()` and `ts_resolve_names()` are renamed `match_autonyms` and
  `match_autonyms_exclude` (#101), since they are about autonyms, the
  infraspecific names that repeat the specific epithet. The old names still
  work as aliases, so existing code is unaffected; giving both an alias and its
  new name with different values is an error.
* **Breaking: match type `auto_cfonly` is renamed `auto_canon` (#82).** The old
  name could be misread as a match on the taxonomic qualifier `cf.`; it means a
  match on the canonical name (genus + specific epithet + infraspecific
  epithet).
* **New match type `auto_orthvar` (#68).** Epithets that differ only by
  standard Latin spelling variants (`sylvestris`/`silvestris`,
  `caerulea`/`coerulea`; `ae`/`oe` = `e`, `y` = `i`, `ph` = `f`, `rh` = `r`,
  `th` = `t`, final `ii` = `i`) are now matched exactly after folding, before
  fuzzy matching, instead of being reported as fuzzy matches. Code that
  filters on `match_type` may see `auto_orthvar` where it saw `auto_fuzzy` or
  `auto_fuzzy_canon`.
* **New `dist` column and optional swap-aware distance (#67).**
  `ts_match_names()` output has a new `dist` column with the edit distance of
  each fuzzy hit (`NA` for other match types; a new column, so code that
  selects columns by position may need updating). The new argument
  `transpositions = TRUE` counts a swap of two adjacent letters (`Qurecus` for
  `Quercus`) as one edit instead of two. It is off by default (plain
  Levenshtein distance, as before, and somewhat faster). With `max_dist = 1`,
  188 swapped-letter PPG queries were all found with it on, versus none with
  it off; at the default `max_dist = 3` the effect is small.
* **Breaking: fuzzy matching of queries without an author (#49).** The author
  string used to count towards the edit distance, so author-less queries could
  only fuzzy-match (loosely, and only because the old default `max_dist` was
  large). They are now fuzzy-matched on the canonical name (genus + specific
  epithet + infraspecific epithet), ignoring the author. A canonical-name match
  is reported as `auto_canon`, and a misspelled one as the new match type
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

* New `ts_format_ref()` turns a table of taxonomic names (for example from a
  downloaded taxonomic database) into a reference: it renames columns with
  `col_map`, adds `scientificNameAuthorship` to `scientificName`, drops ranks
  above species and tidies values (#112, part of #109).
* New `ts_subset_ref()` keeps one or more taxa (for example a family) and their
  synonyms from a large table of names, using higher-rank columns or
  `parentNameUsageID`, so a whole database need not be made into a reference
  (#114, part of #109).
* `ts_format_ref()` now also keeps the columns that give the hierarchy of the
  names (higher ranks such as `family`, and `parentNameUsageID`), so
  `ts_subset_ref()` can be used before or after it for most tables (#121, part
  of #109).
* The argument that holds the reference is now called `ref` in
  `ts_match_names()`, `ts_resolve_names()`, `ts_optimize_ref()`, `ts_format_ref()`
  and `ts_subset_ref()`, so that it is the same everywhere (#120, part of
  #109). The old names, `reference` in `ts_match_names()` and `ref_taxonomy` in
  `ts_resolve_names()`, still work as aliases and will be deprecated in a
  future version. Only calls that name the argument are affected; its position
  is unchanged.
* `ts_format_ref()` now gives a made-up unique `taxonID` to names that have
  none (some databases give none to synonyms), because `ts_resolve_names()`
  could not resolve those names (#117, part of #109).
* New `ts_load_ref()` loads a reference, formats it with `ts_format_ref()`, and
  optionally keeps only some taxa (`taxon`). The data come from a named
  `source` (`"wfo"`, the World Flora Online Plant List; `"ppg"`, the
  Pteridophyte Phylogeny Group classification; or the taxadb snapshots of
  `"itis"`, `"col"`, `"gbif"`, `"ncbi"`, `"ott"`, `"fb"` and `"slb"`, which
  need the nanoparquet or the duckdb package (with duckdb, `taxon` is applied
  while reading, which is much faster for large tables like GBIF); see the new `ts_show_ref_sources()` for
  licenses and citations; with `versions = TRUE` it also lists the versions
  you can download), an HTTPS `url`, or a `file` (CSV/TSV, zipped Darwin Core
  Archive or Parquet). The latest version of a
  named source is used unless a `version` is given, and the version, license
  and citation are recorded in the `ts_source` attribute. Downloads are kept
  in a folder only if you give a `cache_dir` (otherwise they are in
  `tempdir()`) (#115, #116, #117, part of #109).
* **Messages are controlled by one package option, `taxastand.verbosity`
  (#133).** `options(taxastand.verbosity = "quiet")` silences the messages and
  the warnings about names that were excluded (not parsed or placeholders), in
  `ts_load_ref()`, `ts_show_ref_sources()`, `ts_show_ref_cache()`,
  `ts_clear_ref_cache()`, `ts_parse_names()`, `ts_match_names()` and
  `ts_resolve_names()`; the default is `"inform"`. This follows rOpenSci
  advice to set verbosity at the package level. The unreleased `quiet`
  argument of `ts_load_ref()` was removed. `ts_parse_names(quiet = TRUE)` still
  works but is superseded by the option. `ts_load_ref()` now gives one line
  (`Loaded 309 names from ITIS (version 2026), license CC0.`), no longer reports
  the path of a cached file, and gives the citation once per R session for each
  source (it is always in `attr(x, "ts_source")`).
* New `ts_show_ref_cache()` and `ts_clear_ref_cache()` list and delete the files
  downloaded by `ts_load_ref()`; other files in the folder are never touched
  (#132, part of #109).
* `manual_match` in `ts_match_names()` now also works when `query` is a
  dataframe of names parsed with `ts_parse_names()` (#93), not only a character
  vector. The `query` values are compared with the `name` column.
* The documentation of `ts_match_names()` and `ts_resolve_names()` now
  recommends `ts_optimize_ref()` for repeated queries against a large
  reference, and `ts_match_names()` defines an autonym (#94).
* New "Benchmarks" vignette comparing taxastand with similar packages (#96,
  #103): taxastand, taxify, WorldFlora and U.Taxonstand match the same World
  Flora Online reference against queries made by damaging real names (typos in
  the genus, epithet and author, missing authors, homonyms) and by changing the
  form of names in the ways taxastand checks for (a missing basionym author, a
  dropped *ex* element, a changed infraspecific rank, a Latin spelling
  variant), so the right answer is known. It compares accuracy, single answers
  and speed. The results are precomputed and saved with the package, and the
  code that produced them is installed in
  `system.file("benchmark", package = "taxastand")`.
* Faster fuzzy matching: candidates are looked up by reference row rather than
  by name (a 600-query PPG run took 2.4 s, now 2.1 s; results identical).
* Faster parsing and key building (#66): fewer and cheaper regex passes, and
  pipe-delimited records are split once instead of once per field. Output is
  identical; e.g. parsing 149,000 names takes about 1.2 s instead of 3.2 s.
* Lighter installation (#48): dplyr, tidyr, fs and glue are no longer
  dependencies (the resolve and match steps now use base R, with identical
  output), and tibble moved to Suggests. `tbl_out = TRUE` (or the `ts_tbl_out`
  option) now requires the tibble package to be installed. magrittr is also
  no longer a dependency, so **breaking:** `%>%` is no longer re-exported;
  load magrittr or dplyr, or use the native pipe `|>`.
* Document the `match_type` values in a new "Match types" section of
  `?ts_match_names`, grouped by how far each can be trusted (#52); the match
  type codes themselves are unchanged
* Use `vapply()` instead of `sapply()`, `anyDuplicated()` instead of
  `any(duplicated(x))`, and drop several redundant `x == TRUE`/`x == FALSE`
  comparisons
* Fix a dead link in the README ("The Plant List", no longer online; now
  links to a Wayback Machine snapshot)

* New vignette "Using taxadb" shows how to get a reference from a standard
  taxonomic database (ITIS, with a note on others) with the taxadb package and
  match against it (#107).

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
