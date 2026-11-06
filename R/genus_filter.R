# Genus pre-filter for the reference
#
# Every key used by the matcher embeds the genus, and fuzzy matching only
# compares names within a genus, so reference names whose genus cannot match
# any query genus can never match. Dropping them before parsing and key
# building is a large speed-up when the reference is much bigger than the set
# of query genera.
#
# Genera are compared in normalised form (tt_depunct(): lower-cased, punctuation
# and diacritics removed), which is how the matcher's keys compare them, so the
# filter keeps everything that could match under those keys.

# Normalised genus keys to keep, given the parsed query. This is the single
# place to extend the set of genera considered. Query genera that are not in
# the reference are fuzzy-matched to reference genera within `max_dist_genus`
# (see tt_matchnames()), so those neighbouring genera are kept too.
.ts_genus_keep_keys <- function(query_parsed_df, ref_genus_keys = NULL,
                                max_dist_genus = 0) {
  genera <- query_parsed_df$genus_name
  keys <- unique(tt_depunct(genera[!is.na(genera)]))
  if (max_dist_genus > 0 && length(ref_genus_keys)) {
    ref_genus_keys <- unique(ref_genus_keys)
    missing <- setdiff(keys, ref_genus_keys)
    for (g in missing) {
      d <- as.integer(utils::adist(g, ref_genus_keys))
      keys <- c(keys, ref_genus_keys[!is.na(d) & d <= max_dist_genus])
    }
    keys <- unique(keys)
  }
  keys
}

# Cheap genus key from a raw (unparsed) name: its first word, ignoring a
# leading hybrid sign, normalised with tt_depunct().
.ts_raw_genus_key <- function(x) {
  x <- sub("^[[:space:]]+", "", x)
  x <- sub("^(\u00d7[[:space:]]*|x[[:space:]]+)", "", x)
  tt_depunct(sub("[[:space:]].*$", "", x))
}

# Subset raw reference names to those that could match a query genus
.ts_filter_raw_reference <- function(reference, keep_keys) {
  reference[.ts_raw_genus_key(reference) %in% keep_keys]
}

# Subset a parsed reference data frame to those that could match a query genus
.ts_filter_parsed_reference <- function(ref_parsed_df, keep_keys) {
  ref_parsed_df[tt_depunct(ref_parsed_df$genus_name) %in% keep_keys, ]
}

# Zero-row parsed-names data frame (same columns as ts_parse_names() output),
# used when no reference name is in a query genus
.ts_empty_parsed_df <- function() {
  data.frame(
    name = character(),
    id = character(),
    genus_hybrid_sign = character(),
    genus_name = character(),
    species_hybrid_sign = character(),
    specific_epithet = character(),
    infraspecific_rank = character(),
    infraspecific_epithet = character(),
    author = character(),
    stringsAsFactors = FALSE
  )
}
