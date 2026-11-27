# The engine tests write parsed names as `id|genus_hybrid_sign|genus_name|
# species_hybrid_sign|specific_epithet|infraspecific_rank|
# infraspecific_epithet|author` strings, because they are short to read. `rec()`
# turns such strings into the data frame of parsed names the engine takes
# (empty fields are NA, as in ts_parse_names()).
rec <- function(x) {
  fields <- strsplit(paste0(x, strrep("|", 8L)), "|", fixed = TRUE)
  # the author is everything after the 7th pipe
  parts <- lapply(fields, function(f) c(f[1:7], paste(f[-(1:7)], collapse = "|")))
  parts <- lapply(parts, function(f) sub("\\|+$", "", f))
  out <- as.data.frame(
    do.call(rbind, parts), stringsAsFactors = FALSE
  )
  if (!length(x)) {
    out <- as.data.frame(
      matrix(character(), 0, 8), stringsAsFactors = FALSE
    )
  }
  colnames(out) <- c("id", .ts_name_parts)
  out[] <- lapply(out, function(col) {
    col[col == ""] <- NA_character_
    col
  })
  out
}

# tt_matchnames() on parsed names written as strings (see `rec()`)
match_rec <- function(query, ref, ...) {
  tt_matchnames(rec(query), rec(ref), ...)
}
