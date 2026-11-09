# Reduce the result of data-raw/benchmark-vignette.R to the small file read by
# the vignette (benchmark.Rmd): inst/extdata/benchmark-results.rds
#
#   Rscript data-raw/benchmark-combine.R [results.rds [extra.rds ...]]
#
# Run from the package root, after the benchmark run. Each `extra.rds` is the result
# of a later run of some more kinds of error (data-raw/benchmark-vignette.R with
# kinds= and speed=no) on the same reference and machine; its accuracy and
# queries are added to those of `results.rds` (any number of extra files). A tool
# that is in both has its accuracy for the kinds in `extra.rds` replaced, so one
# tool can be rerun alone.

args <- commandArgs(trailingOnly = TRUE)
f <- if (length(args)) args[[1]] else "data-raw/benchmark-results/benchmark-wfo-all.rds"
r <- readRDS(f)
extra_date <- NULL
for (extra_file in args[-1]) {
  extra <- readRDS(extra_file)
  stopifnot(
    identical(extra$reference, r$reference),
    identical(extra$reference_records, r$reference_records),
    identical(extra$versions, r$versions)
  )
  # kinds already in `r` are not added again; the accuracy of a tool in `extra`
  # replaces its accuracy for the kinds that `extra` has (rerun of one tool)
  r$queries <- rbind(
    r$queries, extra$queries[!extra$queries$kind %in% r$queries$kind, ]
  )
  for (tool in intersect(names(extra$accuracy), names(r$accuracy))) {
    old <- r$accuracy[[tool]]
    old <- old[!old$kind %in% extra$accuracy[[tool]]$kind, ]
    r$accuracy[[tool]] <- rbind(old, extra$accuracy[[tool]])
  }
  extra_date <- c(extra_date, format(extra$date, "%Y-%m-%d"))
}
extra_date <- if (length(extra_date)) max(extra_date)
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
  date = format(r$date, "%Y-%m-%d"), extra_date = extra_date,
  reference = r$reference,
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
