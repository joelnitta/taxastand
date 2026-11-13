ref_names <- c(
  "Pteridium aquilinum (L.) Kuhn",
  "Pteris aquilina L.",
  "Asplenium nidus L.",
  "Asplenium nidus var. nidus",
  "Hymenophyllum taiwanense (Tagawa) C.V.Morton",
  "Hymenophyllum taiwanense Tagawa",
  "Cyathea medullaris (G.Forst.) Sw.",
  "Quercus pyrenaica Willd.",
  "Polypodium vulgare L."
)

queries <- c(
  "Asplenium nidus L.", # exact
  "Asplenium nidis L.", # fuzzy
  "Pteridium aquilinim", # fuzzy canonical, no author
  "Aspelnium nidus L.", # fuzzy genus
  "Quercus pirenaica Willd.", # orthographic variant
  "Zzyzx foo Bar" # no match
)

# The opaque reference ID differs between a prepared and a one-off reference
without_ref_id <- function(x) x[, setdiff(colnames(x), "id_ref")]

test_that("a prepared reference gives the same results as the names", {
  prepared <- ts_optimize_ref(ref_names)
  expect_s3_class(prepared, "ts_reference")
  expect_identical(
    without_ref_id(suppressWarnings(ts_match_names(queries, prepared))),
    without_ref_id(suppressWarnings(ts_match_names(queries, ref_names)))
  )
  # any option still works
  expect_identical(
    without_ref_id(suppressWarnings(
      ts_match_names(
        queries, prepared,
        match_canon = TRUE, transpositions = TRUE
      )
    )),
    without_ref_id(suppressWarnings(
      ts_match_names(
        queries, ref_names,
        match_canon = TRUE, transpositions = TRUE
      )
    ))
  )
})

test_that("a prepared reference can be reused and fills in per genus", {
  prepared <- ts_optimize_ref(ref_names)
  expect_output(print(prepared), "9 names in 7 genera")
  expect_output(print(prepared), "genera parsed so far: 0")
  first <- suppressWarnings(
    ts_match_names("Asplenium nidis L.", prepared, simple = TRUE)
  )
  expect_output(print(prepared), "genera parsed so far: 1")
  # a second batch from the same genus reuses it; another genus adds one
  again <- suppressWarnings(
    ts_match_names("Asplenium nidis L.", prepared, simple = TRUE)
  )
  expect_identical(again, first)
  expect_output(print(prepared), "genera parsed so far: 1")
  res <- suppressWarnings(
    ts_match_names(c("Asplenium nidis L.", "Polypodium vulgare L."), prepared,
      simple = TRUE
    )
  )
  expect_identical(res$match_type, c("auto_fuzzy", "exact"))
  expect_output(print(prepared), "genera parsed so far: 2")
})

test_that("records keep their reference order when genera are combined", {
  # Two reference names identical after normalization: the later one wins, as
  # when the reference is given as names
  ref2 <- c(
    "Asplenium nidus L.", "Quercus robur L.", "Asplenium nidus L",
    "Quercus petraea Matt."
  )
  prepared <- ts_optimize_ref(ref2)
  q <- c("Quercus robur L.", "Asplenium nidus L.")
  expect_identical(
    without_ref_id(ts_match_names(q, prepared)),
    without_ref_id(ts_match_names(q, ref2))
  )
})

test_that("ts_resolve_names() accepts a prepared Darwin Core reference", {
  taxonomy <- data.frame(
    taxonID = as.character(1:4),
    scientificName = c(
      "Pteridium aquilinum (L.) Kuhn", "Pteris aquilina L.",
      "Asplenium nidus L.", "Hymenophyllum taiwanense Tagawa"
    ),
    taxonomicStatus = c("accepted", "synonym", "accepted", "accepted"),
    acceptedNameUsageID = c(NA, "1", NA, NA),
    other = "ignored",
    stringsAsFactors = FALSE
  )
  prepared <- ts_optimize_ref(taxonomy)
  q <- c("Pteris aquilina L.", "Asplenium nidis L.", "Zzyzx foo")
  expect_identical(
    suppressWarnings(ts_resolve_names(q, prepared)),
    suppressWarnings(ts_resolve_names(q, taxonomy))
  )
  # also works with the output of ts_match_names()
  matched <- suppressWarnings(ts_match_names(q, prepared, simple = TRUE))
  expect_identical(
    ts_resolve_names(matched, prepared),
    ts_resolve_names(matched, taxonomy)
  )
  # a data.frame prepared reference can be used for matching too
  expect_identical(
    without_ref_id(suppressWarnings(ts_match_names(q, prepared))),
    without_ref_id(suppressWarnings(ts_match_names(q, taxonomy$scientificName)))
  )
})

test_that("names that cannot be parsed are reported once, then skipped", {
  withr::local_options(taxastand.verbosity = "inform")
  ref3 <- c("Asplenium nidus L.", "Asplenium !!! ???", "Polypodium vulgare L.")
  prepared <- ts_optimize_ref(ref3)
  expect_warning(
    ts_match_names("Asplenium nidus L.", prepared),
    "could not be parsed"
  )
  expect_no_warning(ts_match_names("Asplenium nidus L.", prepared))
  # a genus whose names all fail does not error
  prepared2 <- ts_optimize_ref(c("Foo !!! ???", "Polypodium vulgare L."))
  res <- suppressWarnings(ts_match_names("Foo bar", prepared2, simple = TRUE))
  expect_identical(res$match_type, "no_match")
})

test_that("duplicated reference names are an error, as without preparing", {
  dup <- c("Asplenium nidus L.", "Asplenium nidus L.")
  expect_error(
    ts_match_names("Asplenium nidus L.", ts_optimize_ref(dup)),
    "must be unique"
  )
  expect_error(
    ts_match_names("Asplenium nidus L.", dup),
    "must be unique"
  )
})

test_that("strip_qualifiers applies to the reference at preparation", {
  prepared <- ts_optimize_ref(
    c("Tectaria cf. crenata Cav.", "Tectaria zeylanica (Houtt.) Sledge"),
    strip_qualifiers = TRUE
  )
  res <- suppressWarnings(
    ts_match_names("Tectaria crenata Cav.", prepared, simple = TRUE)
  )
  expect_identical(res$match_type, "exact")
  expect_identical(res$reference, "Tectaria cf. crenata Cav.")
})

test_that("ts_optimize_ref() checks its input", {
  expect_error(ts_optimize_ref(1:3), "character vector or data.frame")
  expect_error(ts_optimize_ref(c("a b", NA)), "missing values")
  expect_error(
    ts_optimize_ref(data.frame(name = "a b", stringsAsFactors = FALSE)),
    "scientificName"
  )
  expect_error(
    ts_optimize_ref(ref_names, strip_qualifiers = "yes")
  )
  # names only: cannot resolve synonyms
  expect_error(
    ts_resolve_names("Asplenium nidus L.", ts_optimize_ref(ref_names)),
    "Darwin Core"
  )
  expect_error(
    ts_resolve_names("Asplenium nidus L.", 1),
    "ts_optimize_ref"
  )
  expect_error(
    ts_match_names("Asplenium nidus L.", 1),
    "ts_optimize_ref"
  )
})

test_that("keys build up correctly as genera are added in batches", {
  prepared <- ts_optimize_ref(ref_names)
  batches <- list(
    "Asplenium nidis L.",
    c("Pteridium aquilinum (L.) Kuhn", "Quercus pirenaica Willd."),
    c("Asplenium nidis L.", "Cyathea medullaris Sw.", "Polypodium vulgare")
  )
  for (q in batches) {
    expect_identical(
      without_ref_id(suppressWarnings(ts_match_names(q, prepared))),
      without_ref_id(suppressWarnings(ts_match_names(q, ref_names)))
    )
  }
  # a key that no earlier call needed is built for all the batches added so far
  q <- c("Asplenium nidus", "Hymenophyllum taiwanense", "Quercus pyrenaica")
  expect_identical(
    without_ref_id(suppressWarnings(
      ts_match_names(q, prepared, match_canon = TRUE)
    )),
    without_ref_id(suppressWarnings(
      ts_match_names(q, ref_names, match_canon = TRUE)
    ))
  )
})
