# Real plant names whose author starts with a lowercase word (a particle such
# as "de", "van", "d'"), from IPNI, for tests/testthat/test-author-particles.R.
# Needs the kewr package (https://github.com/barnabywalker/kewr) and internet.
# Run from the package root: source("data-raw/ipni-particle-names.R")

library(kewr)

# 1. All IPNI author standard forms
r <- search_ipni(list(author_std_form = "*"), limit = 500)
authors <- list(tidy(r)[, "standardForm"])
repeat {
  r <- tryCatch(request_next(r), error = function(e) NULL)
  if (is.null(r) || length(r$results) == 0) break
  authors[[length(authors) + 1]] <- tidy(r)[, "standardForm"]
}
std <- unique(unlist(lapply(authors, `[[`, "standardForm")))

# 2. Those that start with a lowercase word
particle_authors <- sort(std[grepl("^[[:lower:]]", std)])

# 3. Species-level and infraspecific names with each of those authors
names_out <- lapply(particle_authors, function(a) {
  res <- tryCatch(
    tidy(search_ipni(list(author = a), limit = 20)),
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res) == 0) {
    return(NULL)
  }
  res <- res[res$rank %in% c("spec.", "var.", "subsp.", "f.") &
    !is.na(res$authors), c("name", "authors")]
  res[startsWith(res$authors, a) | grepl(paste0("[ (]", a), res$authors,
    fixed = TRUE), ]
})
out <- unique(do.call(rbind, names_out))
out <- out[order(out$authors, out$name), ]
write.csv(out, "tests/testthat/fixtures/ipni-particle-names.csv",
  row.names = FALSE, fileEncoding = "UTF-8")
