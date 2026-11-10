# Precompute the results shown in vignettes/taxadb.Rmd.
#
# Not run on CI or when the vignette is built: it needs the taxadb package and
# a network connection. Re-run it by hand to refresh
# the saved results in inst/extdata (taxadb-example.rds).
#
#   Rscript data-raw/taxadb-vignette.R

library(taxadb)
library(dplyr)
devtools::load_all(quiet = TRUE)

# ---- get-table ----
itis <- taxa_tbl(provider = "itis", schema = "dwc")
# ---- end ----

# ---- make-reference ----
ref <- itis |>
  filter(family == "Polypodiaceae") |>
  collect() |>
  mutate(
    # taxadb keeps the author in its own column; taxastand wants it in
    # scientificName
    scientificName = ifelse(
      is.na(scientificNameAuthorship),
      scientificName,
      paste(scientificName, scientificNameAuthorship)
    )
  ) |>
  select(taxonID, acceptedNameUsageID, taxonomicStatus, scientificName)
# ---- end ----

# ---- resolve ----
ref_prepared <- ts_prepare_reference(ref)
queries <- c(
  "Polypodium vulgare L.",
  "Phymatodes scolopendria (Burm. f.) Ching",
  "Microsorum scolopendria",
  "Pleopeltis polylepis var. erythrolepis (Weath.) T. Wendt",
  "Polypodium pelucidum Kaulf."
)
resolved <- ts_resolve_names(queries, ref_prepared, match_no_auth = TRUE)
# ---- end ----

out <- list(
  taxadb_version = as.character(packageVersion("taxadb")),
  snapshot = available_versions()[1],
  provider = "itis",
  n_names = nrow(ref),
  reference_head = head(ref[ref$taxonomicStatus == "synonym" & lengths(strsplit(ref$scientificName, " ")) > 2, ], 5),
  status_counts = count(ref, taxonomicStatus),
  queries = queries,
  resolved = as.data.frame(resolved)[match(queries, resolved$query), ],
  date = Sys.Date()
)
saveRDS(out, "inst/extdata/taxadb-example.rds", compress = "xz")
print(out[c("taxadb_version", "snapshot", "n_names")])
print(out$resolved)
