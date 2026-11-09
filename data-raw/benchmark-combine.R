# Reduce the result of data-raw/benchmark-vignette.R to the small file read by
# the vignette (benchmark.Rmd): inst/extdata/benchmark-results.rds
#
#   Rscript data-raw/benchmark-combine.R [results.rds]
#
# Run from the package root, after the benchmark run.

f <- commandArgs(trailingOnly = TRUE)
f <- if (length(f)) f[[1]] else "data-raw/benchmark-results/benchmark-wfo-all.rds"
r <- readRDS(f)
# one example per kind of error for the vignette: the shortest name of at least
# 18 characters, so that the change is easy to see
q <- r$queries
ex <- do.call(rbind, lapply(split(q, q$kind), function(d) {
  d <- d[nchar(d$original) >= 18, ]
  d <- d[order(nchar(d$original)), ]
  d[1, c("kind", "original", "query")]
}))
rownames(ex) <- NULL
# the session of the run; results saved before the runner recorded it get the
# session of this machine, with the compared packages loaded
si <- r$session_info
if (is.null(si)) {
  for (p in c("taxify", "WorldFlora", "U.Taxonstand", "vectra")) loadNamespace(p)
  si <- utils::capture.output(utils::sessionInfo())
}
res <- list(
  date = format(r$date, "%Y-%m-%d"), reference = r$reference,
  reference_records = r$reference_records, n_per_kind = r$n_per_kind,
  n_queries = table(r$queries$kind), examples = ex, tools = r$tools,
  accuracy = r$accuracy, speed = r$speed, setup_s = r$setup_s,
  first_run_s = r$first_run_s, versions = r$versions,
  r_version = r$r_version, platform = r$platform,
  session_info = si
)
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(res, "inst/extdata/benchmark-results.rds", compress = "xz")
cat(
  "Wrote inst/extdata/benchmark-results.rds (",
  file.size("inst/extdata/benchmark-results.rds"), "bytes )\n"
)
