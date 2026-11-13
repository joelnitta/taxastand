# Reproducible speed benchmark for ts_resolve_names() against PPG.
#
# Usage: Rscript data-raw/benchmark-ppg.R [path/to/ppg.csv] [max_dist]
# If no path is given, PPG v2.0.0 is downloaded to a temp file. Run from the
# package root; uses devtools::load_all() so it measures the working tree.

args <- commandArgs(trailingOnly = TRUE)
ppg_version <- "v2.0.0"
ppg_path <- if (length(args) >= 1) args[[1]] else file.path(tempdir(),
  "ppg.csv")
max_dist <- if (length(args) >= 2) as.numeric(args[[2]]) else 3

if (!file.exists(ppg_path)) {
  utils::download.file(
    sprintf(
      "https://raw.githubusercontent.com/pteridogroup/ppg/%s/data/ppg.csv",
      ppg_version
    ),
    ppg_path, quiet = TRUE
  )
}

suppressMessages(devtools::load_all(quiet = TRUE))
ppg <- utils::read.csv(ppg_path, stringsAsFactors = FALSE, na.strings = "NA")
ppg <- ppg[!is.na(ppg$scientificName), ]
# ts_resolve_names() expects authorship inside scientificName; PPG keeps it in
# a separate column, so paste them together (as the PPG website resolver does)
has_auth <- !is.na(ppg$scientificNameAuthorship)
ppg$scientificName[has_auth] <- paste(
  ppg$scientificName[has_auth], ppg$scientificNameAuthorship[has_auth]
)
cat("Reference rows:", nrow(ppg), "\n")

queries <- c(
  "Crepidomanes minutum (Blume) K.Iwats.", # 1-char typo in epithet, with author
  "Asplenium nidus L.",                     # exact
  "Cyathea medullaris (G.Forst.) Sw.",      # exact
  "Pteris vitata",                          # author-less
  "Polypodium vulgare L."                   # exact
)

start <- proc.time()
res <- ts_resolve_names(queries, ppg, max_dist = max_dist)
t <- (proc.time() - start)[["elapsed"]]
cat(sprintf("max_dist = %s: %.1f s, %d result rows\n", max_dist, t, nrow(res)))
print(table(res$match_type))
