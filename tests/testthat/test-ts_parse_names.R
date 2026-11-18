test_that("Input checks work", {
  expect_error(
    ts_parse_names(c("Foogenus", "Foogenus")),
    "Input taxa must be unique"
  )
  expect_error(
    ts_parse_names(c("Foogenus", NA)),
    "Input taxa may not contain NAs"
  )
})

test_that("Parsing works", {
  expect_snapshot(
    ts_parse_names("Foogenus x barspecies var. foosubsp (L.) F. Bar")
  )
})

test_that("strip_qualifiers = FALSE (default) mis-parses open qualifiers", {
  cf_parsed <- ts_parse_names("Tectaria cf. crenata", quiet = TRUE)
  expect_identical(cf_parsed$specific_epithet, "cf")
  expect_identical(cf_parsed$author, ". crenata")

  aff_parsed <- ts_parse_names("Huperzia aff. liangshanica", quiet = TRUE)
  expect_identical(aff_parsed$specific_epithet, "aff")
  expect_identical(aff_parsed$author, ". liangshanica")
})

test_that("strip_qualifiers = TRUE strips cf. and aff. before parsing", {
  cf_parsed <- ts_parse_names(
    "Tectaria cf. crenata",
    strip_qualifiers = TRUE
  )
  expect_identical(cf_parsed$specific_epithet, "crenata")
  expect_true(is.na(cf_parsed$author))

  aff_parsed <- ts_parse_names(
    "Huperzia aff. liangshanica",
    strip_qualifiers = TRUE
  )
  expect_identical(aff_parsed$specific_epithet, "liangshanica")
  expect_true(is.na(aff_parsed$author))
})

test_that("strip_qualifiers = TRUE strips a qualifier before an epithet", {
  parsed <- ts_parse_names(
    "Foogenus barspecies aff. var. foosubsp",
    strip_qualifiers = TRUE
  )
  expect_identical(parsed$specific_epithet, "barspecies")
  expect_identical(parsed$infraspecific_rank, "var.")
  expect_identical(parsed$infraspecific_epithet, "foosubsp")
})

test_that("strip_qualifiers = TRUE strips trailing sensu lato/stricto", {
  sl_parsed <- ts_parse_names(
    "Foogenus barspecies s.l.",
    strip_qualifiers = TRUE
  )
  expect_identical(sl_parsed$specific_epithet, "barspecies")
  expect_true(is.na(sl_parsed$author))

  sstr_parsed <- ts_parse_names(
    "Foogenus barspecies sensu stricto",
    strip_qualifiers = TRUE
  )
  expect_identical(sstr_parsed$specific_epithet, "barspecies")
  expect_true(is.na(sstr_parsed$author))
})

test_that("lowercase author particles stay in the author (#170)", {
  taxa <- c(
    "Angiopteris affinis de Vriese",
    "Aiouea grandifolia van der Werff",
    "Thelypteris nubicola de la Sota",
    "Allium graecum d'Urv.",
    "Aceras pyramidale var. brachystachys (d'Urv.) Rchb.f.",
    "Foo bar var. baz de Vriese",
    "Foo bar d Smith",
    "Abies carpatica hort.",
    "Abies conica hort. ex Lav\u00e9e",
    "Foo bar auct.",
    "Foo bar auct. non L."
  )
  res <- suppressWarnings(ts_parse_names(taxa, quiet = TRUE))
  expect_identical(res$name, taxa)
  expect_identical(
    res$author,
    c(
      "de Vriese", "van der Werff", "de la Sota", "d'Urv.",
      "(d'Urv.) Rchb.f.", "de Vriese", "d Smith", "hort.",
      "hort. ex Lav\u00e9e", "auct.", "auct. non L."
    )
  )
  expect_identical(
    res$infraspecific_epithet,
    c(NA, NA, NA, NA, "brachystachys", "baz", rep(NA, 5))
  )
})

test_that("particles that are not in IPNI author names are kept too (#170)", {
  # Queries need not come from IPNI
  taxa <- c(
    "Foo bar dos Santos", "Foo bar della Torre", "Foo bar le Roux",
    "Foo bar zu Pfeffer", "Foo bar von der Decken", "Foo bar af Ugglas",
    "Foo bar var. baz el Hadidi"
  )
  res <- suppressWarnings(ts_parse_names(taxa, quiet = TRUE))
  expect_identical(
    res$author,
    c(
      "dos Santos", "della Torre", "le Roux", "zu Pfeffer", "von der Decken",
      "af Ugglas", "el Hadidi"
    )
  )
  expect_identical(res$infraspecific_epithet, c(rep(NA, 6), "baz"))
})

test_that("'ex', 'sensu' and 'non' at the start of the author are not epithets (#170)", {
  taxa <- c(
    "Verbesina conyzoides ex DC.",
    "Mikania aschersonii sensu Pruski & non Hieron.",
    "Blepharis acaulis non Lindau"
  )
  res <- suppressWarnings(ts_parse_names(taxa, quiet = TRUE))
  expect_identical(
    res$author,
    c("ex DC.", "sensu Pruski & non Hieron.", "non Lindau")
  )
  expect_true(all(is.na(res$infraspecific_epithet)))
})

test_that("a particle with no author after it cannot be parsed (#170)", {
  withr::local_options(taxastand.verbosity = "inform")
  expect_warning(
    res <- ts_parse_names(c("Foo bar de", "Foo bar var. van der", "Foo bar"),
      exclude_placeholders = FALSE
    ),
    "could not be parsed"
  )
  expect_identical(res$name, "Foo bar")
})

test_that("a particle written directly before the capital stays in the author (#170)", {
  res <- suppressWarnings(ts_parse_names("Foo bar duQuesnay", quiet = TRUE))
  expect_identical(res$author, "duQuesnay")
  expect_true(is.na(res$infraspecific_epithet))
})

test_that("real IPNI names with a lowercase author particle parse correctly (#170)", {
  # Names whose IPNI author starts with a lowercase word, made by
  # data-raw/ipni-particle-names.R (kewr, 2026-11)
  fx <- read.csv(
    test_path("fixtures", "ipni-particle-names.csv"),
    encoding = "UTF-8"
  )
  fx$full <- paste(fx$name, fx$authors)
  fx <- fx[!duplicated(fx$full), ]
  res <- suppressWarnings(ts_parse_names(fx$full, quiet = TRUE))
  expect_identical(nrow(res), nrow(fx))
  expect_identical(res$author, fx$authors[match(res$name, fx$full)])
})
