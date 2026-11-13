test_that("Input checks work", {
  expect_error(
    ts_match_names(10, "Foogenus"),
    "query must be of class"
  )
  expect_error(
    ts_match_names("Foogenus", 10),
    "ref must be of class"
  )
  expect_error(
    ts_match_names(10, data.frame(genus = "Foogenus",
      stringsAsFactors = FALSE)),
    "query must be of class"
  )
  expect_error(
    ts_match_names(data.frame(genus = "Foogenus", stringsAsFactors = FALSE),
      10),
    "ref must be of class"
  )
})

test_that("Produces expected output", {
  match_res <- ts_match_names(
    "Crepidomanes minutus",
    "Crepidomanes minutum"
  )
  expect_s3_class(match_res, "data.frame")
  expect_snapshot(match_res)
})

test_that("Manually matched names work", {
  match_res <- ts_match_names(
    query = c("Crepidomanes minutus", "Hymeefee erae"),
    ref = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
    manual_match = data.frame(
      query = "Hymeefee erae",
      match = "Hymenophyllum polyanthos",
      stringsAsFactors = FALSE
    ),
    simple = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Names that can't be parsed don't show up in results", {
  match_res <- ts_match_names(
    query = c(
      "Vanden kalamocarpa x Vanden nipponica x Vanden striata",
      "Crepidomanes minutus"
    ),
    ref = "Crepidomanes minutum",
    simple = TRUE,
    tbl_out = TRUE
  )
  expect_snapshot(match_res)
})

test_that("Manually matched names work with collapsed infrasp names", {
  match_res <- ts_match_names(
    query = c(
      "Crepidomanes minutus",
      "Crepidomanes minutawtaw",
      "Blechnum lunare var. lunare",
      "Blechnum lunare",
      "Bar foo var. foo",
      "Bar foo"
    ),
    ref = c(
      "Crepidomanes minutum",
      "Hymenophyllum polyanthos",
      "Blechnum lunare",
      "Bar foo"
    ),
    manual_match = data.frame(
      query = c("Bar foo var. foo", "Crepidomanes minutawtaw"),
      match = c("Bar foo", "Crepidomanes minutum"),
      stringsAsFactors = FALSE
    ),
    max_dist = 10,
    match_canon = FALSE,
    match_autonyms = TRUE,
    match_autonyms_exclude = NULL,
    simple = TRUE,
    tbl_out = TRUE
  )
  expect_snapshot(match_res)
})

test_that("collapsed autonyms that match exactly get match_type 'autonym'", {
  ref <- c("Blechnum lunare", "Foo bar")
  res <- ts_match_names(
    c(
      "Blechnum lunare var. lunare", "Blechnum lunare", "Foo bar var. bar",
      "Blechnum lunarr var. lunarr"
    ),
    ref,
    match_autonyms = TRUE, simple = TRUE
  )
  expect_identical(
    res$match_type,
    c("autonym", "exact", "autonym", "auto_fuzzy_canon")
  )
  # the plain species name is still exact, and the queries keep their text
  expect_identical(res$reference[1:2], c("Blechnum lunare", "Blechnum lunare"))
  expect_identical(res$query[1], "Blechnum lunare var. lunare")
  # an excluded name is not collapsed, so it matches exactly if it is present
  res_ex <- ts_match_names(
    "Blechnum lunare var. lunare", c(ref, "Blechnum lunare var. lunare"),
    match_autonyms = TRUE,
    match_autonyms_exclude = "Blechnum lunare var. lunare", simple = TRUE
  )
  expect_identical(res_ex$match_type, "exact")
  # without match_autonyms the autonym only matches if the reference has it
  res_no <- ts_match_names(
    "Blechnum lunare var. lunare", c(ref, "Blechnum lunare var. lunare"),
    simple = TRUE
  )
  expect_identical(res_no$match_type, "exact")
})

test_that("collapse_infra and collapse_infra_exclude still work as aliases", {
  q <- c("Blechnum lunare var. lunare", "Blechnum lunare", "Foo bar var. bar")
  ref <- c("Blechnum lunare", "Foo bar")
  new <- ts_match_names(
    q, ref,
    match_autonyms = TRUE, match_autonyms_exclude = "Foo bar var. bar"
  )
  old <- ts_match_names(
    q, ref,
    collapse_infra = TRUE, collapse_infra_exclude = "Foo bar var. bar"
  )
  expect_identical(old, new)
  expect_identical(old$match_type[1], "autonym")
  # the same value under both names is fine; different values are an error
  expect_identical(
    ts_match_names(q, ref, match_autonyms = TRUE, collapse_infra = TRUE),
    ts_match_names(q, ref, match_autonyms = TRUE)
  )
  expect_error(
    ts_match_names(q, ref, match_autonyms = TRUE, collapse_infra = FALSE),
    "alias of `match_autonyms`"
  )
  expect_error(
    ts_match_names(
      q, ref,
      match_autonyms = TRUE, match_autonyms_exclude = "Foo bar var. bar",
      collapse_infra_exclude = "Blechnum lunare var. lunare"
    ),
    "alias of `match_autonyms_exclude`"
  )
  # an alias is validated like the new name
  expect_error(
    ts_match_names("Foo bar", "Foo bar", collapse_infra_exclude = 1),
    "character vector"
  )
})

test_that("strip_qualifiers = TRUE resolves an open qualifier correctly", {
  match_res <- ts_match_names(
    query = "Tectaria cf. crenata",
    ref = c("Tectaria crenata", "Tectaria subvariolosa"),
    strip_qualifiers = TRUE,
    simple = TRUE
  )
  expect_identical(match_res$reference, "Tectaria crenata")
  expect_identical(match_res$match_type, "exact")
})

test_that("Incorrectly specified manual match fails", {
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      ref = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = "Hymeefee erae",
        match = "Hymenophyllum poWHAT",
        stringsAsFactors = FALSE
      ),
      simple = TRUE
    ),
    "One or more manually matched reference names not in reference data"
  )
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      ref = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        query = c("Crepidomanes minutus", "Crepidomanes minutus"),
        match = c("Hymenophyllum polyanthos", "Crepidomanes minutum"),
        stringsAsFactors = FALSE
      ),
      simple = TRUE
    ),
    "All values of manual_match\\$query must be unique"
  )
  expect_error(
    ts_match_names(
      query = c("Crepidomanes minutus", "Hymeefee erae"),
      ref = c("Crepidomanes minutum", "Hymenophyllum polyanthos"),
      manual_match = data.frame(
        name = c("Hymenophyllum polyantha", "Crepidomanes minutu"),
        match = c("Hymenophyllum polyanthos", "Crepidomanes minutum"),
        stringsAsFactors = FALSE
      ),
      simple = TRUE
    ),
    "manual_match must have `query` and `match` columns"
  )
})

test_that("manual_match works when the query is a parsed dataframe", {
  q <- c("Crepidomanes minutus", "Hymeefee erae", "Hymenophyllum polyantha")
  ref <- c("Crepidomanes minutum", "Hymenophyllum polyanthos")
  manual <- data.frame(
    query = c("Hymeefee erae", "Not in query"),
    match = c("Hymenophyllum polyanthos", "Crepidomanes minutum"),
    stringsAsFactors = FALSE
  )
  cols <- c("query", "reference", "match_type", "dist")
  from_chr <- ts_match_names(q, ref, manual_match = manual)
  from_df <- ts_match_names(ts_parse_names(q), ref, manual_match = manual)
  expect_identical(from_df[, cols], from_chr[, cols])
  expect_identical(
    from_df$match_type[from_df$query == "Hymeefee erae"], "manual"
  )
  expect_identical(
    from_df$reference[from_df$query == "Hymeefee erae"],
    "Hymenophyllum polyanthos"
  )
  # the other names are matched as usual
  others <- from_df$match_type[from_df$query != "Hymeefee erae"]
  expect_false("manual" %in% others)
})

test_that("manual_match with a parsed query works with collapsed infrasp", {
  q <- c(
    "Crepidomanes minutus", "Crepidomanes minutawtaw",
    "Blechnum lunare var. lunare", "Blechnum lunare",
    "Bar foo var. foo", "Bar foo"
  )
  ref <- c(
    "Crepidomanes minutum", "Hymenophyllum polyanthos", "Blechnum lunare",
    "Bar foo"
  )
  manual <- data.frame(
    query = c("Bar foo var. foo", "Crepidomanes minutawtaw"),
    match = c("Bar foo", "Crepidomanes minutum"),
    stringsAsFactors = FALSE
  )
  args <- list(
    ref = ref, manual_match = manual, max_dist = 10,
    collapse_infra = TRUE, simple = TRUE
  )
  from_chr <- do.call(ts_match_names, c(list(query = q), args))
  from_df <- do.call(ts_match_names, c(list(query = ts_parse_names(q)), args))
  expect_identical(from_df, from_chr)
})

test_that("manual_match with a parsed query respects strip_qualifiers", {
  q <- "Tectaria cf. zzzz"
  ref <- c("Tectaria crenata", "Tectaria subvariolosa")
  # the query name is compared as given (the qualifier is kept in `name`)
  manual <- data.frame(query = "Tectaria cf. zzzz", match = "Tectaria crenata",
    stringsAsFactors = FALSE)
  res <- ts_match_names(
    ts_parse_names(q, strip_qualifiers = TRUE), ref,
    manual_match = manual, strip_qualifiers = TRUE, simple = TRUE
  )
  expect_identical(res$match_type, "manual")
  expect_identical(res$reference, "Tectaria crenata")
})

test_that("a manually matched name that cannot be parsed is an error", {
  ref <- c("Crepidomanes minutum", "Foo !!! ???")
  expect_error(
    ts_match_names(
      ts_parse_names("Hymeefee erae"), ref,
      manual_match = data.frame(query = "Hymeefee erae", match = "Foo !!! ???",
        stringsAsFactors = FALSE)
    ),
    "could not be parsed"
  )
})

test_that("ts_match_names accepts pre-parsed query and reference", {
  q <- c("Crepidomanes minutus", "Hymenophyllum polyanthos")
  r <- c("Crepidomanes minutum", "Hymenophyllum polyanthos")
  direct <- ts_match_names(q, r, simple = TRUE)
  parsed <- ts_match_names(
    ts_parse_names(q), ts_parse_names(r), simple = TRUE
  )
  expect_identical(parsed, direct)
  expect_identical(
    ts_match_names(q, ts_parse_names(r), simple = TRUE), direct
  )
})

test_that("ts_match_names validates match_autonyms_exclude", {
  expect_error(
    ts_match_names("Foo bar", "Foo bar", match_autonyms_exclude = 1),
    "character vector"
  )
})

test_that("tbl_out = TRUE returns a tibble", {
  skip_if_not_installed("tibble")
  q <- "Crepidomanes minutus"
  r <- "Crepidomanes minutum"
  expect_s3_class(ts_parse_names(q, tbl_out = TRUE), "tbl_df")
  expect_s3_class(ts_match_names(q, r, tbl_out = TRUE), "tbl_df")
  data(filmy_taxonomy)
  expect_s3_class(
    ts_resolve_names("Gonocormus minutum", filmy_taxonomy, tbl_out = TRUE),
    "tbl_df"
  )
})

test_that("tbl_out = TRUE without tibble installed gives a clear error", {
  local_mocked_bindings(requireNamespace = function(...) FALSE,
    .package = "base")
  expect_error(
    ts_parse_names("Crepidomanes minutum", tbl_out = TRUE),
    "tibble"
  )
})

test_that("`reference` still works as an alias of `ref`", {
  q <- "Gonocormus minutum"
  r <- unique(filmy_taxonomy$scientificName)
  expect_identical(
    ts_match_names(q, reference = r, simple = TRUE),
    ts_match_names(q, ref = r, simple = TRUE)
  )
  expect_error(
    ts_match_names(q, ref = r, reference = "Other name", simple = TRUE),
    "alias of `ref`"
  )
})

test_that("`match_no_auth` still works as an alias of `match_canon`", {
  .ts_session$no_auth_told <- TRUE
  q <- "Foo bar Zed."
  ref <- "Foo bar L."
  expect_identical(
    ts_match_names(q, ref, max_dist = 0)$match_type, "no_match"
  )
  old <- ts_match_names(q, ref, max_dist = 0, match_no_auth = TRUE)
  expect_identical(old$match_type, "auto_canon")
  expect_identical(old, ts_match_names(q, ref, max_dist = 0,
    match_canon = TRUE))
  expect_identical(
    ts_resolve_names(q, data.frame(
      taxonID = "1", acceptedNameUsageID = NA_character_,
      taxonomicStatus = "accepted", scientificName = ref,
      stringsAsFactors = FALSE
    ), max_dist = 0, match_no_auth = TRUE)$resolved_name,
    ref
  )
  expect_error(
    ts_match_names(q, ref, match_canon = FALSE, match_no_auth = TRUE),
    "alias of `match_canon`"
  )
})

test_that("using `match_no_auth` says once that it is an alias", {
  withr::local_options(taxastand.verbosity = "inform")
  .ts_session$no_auth_told <- NULL
  expect_message(
    ts_match_names("Foo bar", "Foo bar", match_no_auth = TRUE),
    "alias of `match_canon`"
  )
  expect_no_message(ts_match_names("Foo bar", "Foo bar", match_no_auth = TRUE))
  .ts_session$no_auth_told <- NULL
  expect_no_message(ts_match_names("Foo bar", "Foo bar"))
})

test_that("match_autonyms also finds an autonym in the reference", {
  ref <- c("Blechnum lunare var. lunare (Sw.) Hook.", "Foo bar subsp. bar L.")
  res <- ts_match_names(
    c(
      "Blechnum lunare (Sw.) Hook.", "Blechnum lunare", "Foo bar L.",
      "Quercus robur"
    ),
    ref,
    match_autonyms = TRUE
  )
  expect_identical(res$match_type, c("autonym", "auto_canon", "autonym",
    "no_match"))
  expect_identical(res$reference[1:3], c(ref[1], ref[1], ref[2]))
  # the query columns describe the query, not the variant that was tried
  expect_true(all(is.na(res$infraspecific_epithet_query)))
  expect_identical(res$infraspecific_epithet_ref[1], "lunare")
  # off by default
  expect_identical(
    ts_match_names("Blechnum lunare (Sw.) Hook.", ref)$match_type, "no_match"
  )
  # an excluded name is not tried as an autonym
  expect_identical(
    ts_match_names(
      "Blechnum lunare (Sw.) Hook.", ref,
      match_autonyms = TRUE,
        match_autonyms_exclude = "Blechnum lunare (Sw.) Hook."
    )$match_type,
    "no_match"
  )
})

test_that("match_autonyms keeps a species match over a reference autonym", {
  ref <- c("Blechnum lunare (Sw.) Hook.",
    "Blechnum lunare var. lunare (Sw.) Hook.")
  res <- ts_match_names("Blechnum lunare (Sw.) Hook.", ref,
    match_autonyms = TRUE)
  expect_identical(res$match_type, "exact")
  expect_identical(res$reference, ref[1])
})

test_that("an autonym query matches an autonym in the reference literally", {
  ref <- "Blechnum lunare var. lunare (Sw.) Hook."
  res <- ts_match_names(ref, ref, match_autonyms = TRUE)
  expect_identical(res$match_type, "exact")
  expect_identical(res$reference, ref)
})

test_that("ts_resolve_names resolves an autonym in the reference", {
  tax <- data.frame(
    taxonID = c("1", "2"), acceptedNameUsageID = c(NA, "1"),
    taxonomicStatus = c("accepted", "synonym"),
    scientificName = c("Foo bar L.", "Foo baz subsp. baz Sm."),
    stringsAsFactors = FALSE
  )
  res <- ts_resolve_names("Foo baz Sm.", tax, match_autonyms = TRUE)
  expect_identical(res$resolved_name, "Foo bar L.")
  expect_identical(res$match_type, "autonym")
})

test_that("reference autonyms are found for spelling variants and all forms", {
  ref <- c("Foo silvestris var. silvestris L.", "Bar baz L.")
  res <- ts_match_names("Foo sylvestris L.", ref, match_autonyms = TRUE)
  expect_identical(res$match_type, "auto_orthvar")
  expect_identical(res$reference, ref[1])
  # the same result for a prepared reference and for parsed reference names
  prep <- ts_optimize_ref(ref)
  expect_identical(
    ts_match_names("Foo silvestris L.", prep, match_autonyms = TRUE)$match_type,
    "autonym"
  )
  expect_identical(
    ts_match_names(
      "Foo silvestris L.", ts_parse_names(ref), match_autonyms = TRUE
    )$match_type,
    "autonym"
  )
  # a reference without autonyms is not affected
  expect_identical(
    ts_match_names("Quercus robur L.", ref, match_autonyms = TRUE)$match_type,
    "no_match"
  )
})

test_that("a manual_match with no matching query changes nothing", {
  ref <- c("Pteris vittata L.", "Pteris cretica L.")
  parsed <- ts_parse_names("Pteris vittata L.")
  plain <- ts_match_names(parsed, ref)
  with_manual <- ts_match_names(
    parsed, ref,
    manual_match = data.frame(
      query = "Nothing here", match = "Pteris cretica L.",
      stringsAsFactors = FALSE
    )
  )
  expect_identical(with_manual[colnames(plain)], plain)
})

test_that("match_autonyms leaves a query alone when no variant matches", {
  ref <- c("Blechnum lunare var. lunare (L.) Bar", "Pteris vittata L.")
  res <- ts_match_names(
    "Blechnum lunare Zzzzzzzzzz", ref,
    match_autonyms = TRUE, match_canon = FALSE, max_dist = 0
  )
  expect_identical(res$match_type, "no_match")
})
