# Paired benchmark for the user-facing vignette (vignettes/benchmark.Rmd).
#
# Not part of the package and not run in CI: it needs taxify (with a downloaded
# backbone), WorldFlora and U.Taxonstand installed. Run by hand from the package
# root; it measures the working tree (devtools::load_all()):
#
#   Rscript data-raw/benchmark-vignette.R reference=ppg n=100
#   Rscript data-raw/benchmark-vignette.R reference=wfo n=100
#     tools=taxastand,taxify
#
# Arguments (key=value, all optional)
#   reference  ppg (default) or wfo. The shared reference table. "wfo" is read
#              from taxify's installed backbone, so it is what taxify matches.
#   n          queries per kind of error (default 100)
#   tools      comma-separated subset of taxastand, taxastand_oneoff, taxify,
#              utaxonstand, worldflora. taxastand_oneoff passes the names to
#              ts_match_names() on every call instead of a prepared reference.
#              Default: ppg -> taxastand,utaxonstand,worldflora;
#              wfo -> taxastand,taxify
#   sizes      query counts for the speed test (default 1,10,100,1000)
#   kinds      comma-separated kinds of error to run (default: all). The queries
#              of a kind do not depend on which other kinds are run, so a
#              later run of some kinds can be merged into an earlier one
#              (see benchmark-combine.R)
#   speed      yes (default) or no: whether to time the tools
#   slow_sizes query counts for the speed test of utaxonstand and worldflora,
#              which take minutes per 1000 queries on a large reference
#              (default 1,10,100)
#   ppg        path to ppg.csv (downloaded when missing)
#   out        output file (default
#              data-raw/benchmark-results/benchmark-<reference>.rds)
#
# Every tool matches against the same reference table (see
# inst/benchmark/benchmark-lib.R), so each comparison is paired: only the
# matching differs. The output is a list; copy the final results to
# inst/extdata/benchmark-results.rds for the vignette.

args <- strsplit(commandArgs(trailingOnly = TRUE), "=", fixed = TRUE)
args <- stats::setNames(
  vapply(args, `[`, "", 2), vapply(args, `[`, "", 1)
)
opt <- function(name, default) {
  if (name %in% names(args)) args[[name]] else default
}
reference <- opt("reference", "ppg")
n <- as.integer(opt("n", 100))
tools <- strsplit(
  opt("tools",
    if (reference == "ppg") {
      "taxastand,utaxonstand,worldflora"
    } else {
      "taxastand,taxify"
    }
  ),
  ",",
  fixed = TRUE
)[[1]]
sizes <- as.integer(strsplit(opt("sizes", "1,10,100,1000"), ",",
  fixed = TRUE)[[1]])
slow_sizes <- as.integer(strsplit(opt("slow_sizes", "1,10,100"), ",",
  fixed = TRUE)[[1]])
kinds <- if ("kinds" %in% names(args)) strsplit(args[["kinds"]], ",",
  fixed = TRUE)[[1]]
do_speed <- identical(opt("speed", "yes"), "yes")
out <- opt("out", sprintf("data-raw/benchmark-results/benchmark-%s.rds",
  reference))
dir.create(dirname(out), showWarnings = FALSE, recursive = TRUE)

suppressMessages(devtools::load_all(quiet = TRUE))
source("inst/benchmark/benchmark-lib.R")

ref <- load_reference(reference, ppg_path = opt("ppg", NULL))
cat("Reference:", reference, " records:", nrow(ref), "\n")
queries <- make_queries(ref, n = n)
if (!is.null(kinds)) {
  stopifnot(all(kinds %in% queries$kind))
  queries <- queries[queries$kind %in% kinds, ]
}
cat("Queries:", nrow(queries), "\n")
print(table(queries$kind))

builders <- list(
  taxastand = function() make_taxastand(ref),
  taxastand_oneoff = function() make_taxastand(ref, prepared = FALSE),
  taxify = function() make_taxify(ref),
  utaxonstand = function() make_utaxonstand(ref),
  worldflora = function() make_worldflora(ref)
)
stopifnot(all(tools %in% names(builders)))

accuracy <- list()
speed <- list()
setup <- list()
first_run <- list()
for (tool in tools) {
  cat("\n==", tool, "==\n")
  adapter <- builders[[tool]]()
  setup[[tool]] <- adapter$setup_s
  cat("setup:", round(adapter$setup_s, 1), "s\n")
  start <- proc.time()
  hits <- adapter$run(queries$query)
  t_run <- (proc.time() - start)[["elapsed"]]
  cat("matched", nrow(queries), "queries in", round(t_run, 1),
    "s (first run)\n")
  first_run[[tool]] <- t_run
  scores <- score_queries(queries, hits)
  accuracy[[tool]] <- summarize_scores(scores)
  print(accuracy[[tool]])
  if (do_speed) {
    tool_sizes <- if (tool %in% c("utaxonstand",
      "worldflora")) slow_sizes else sizes
    speed[[tool]] <- cbind(tool = tool, time_adapter(adapter, queries$query,
      tool_sizes))
    print(speed[[tool]])
  }
}

res <- list(
  date = Sys.time(), reference = reference, reference_records = nrow(ref),
  n_per_kind = n, queries = queries[, c("query", "kind", "original")],
  tools = tools, accuracy = accuracy, speed = speed, setup_s = setup,
  first_run_s = first_run,
  versions = c(
    taxastand = as.character(utils::packageVersion("taxastand")),
    vapply(
      Filter(
        function(p) nzchar(system.file(package = p)),
        c("taxify", "WorldFlora", "U.Taxonstand")
      ),
      function(p) as.character(utils::packageVersion(p)), ""
    )
  ),
  r_version = R.version.string, platform = R.version$platform,
  session_info = utils::capture.output(utils::sessionInfo())
)
saveRDS(res, out)
cat("\nResults written to", out, "\n")
