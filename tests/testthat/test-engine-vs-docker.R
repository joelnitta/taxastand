# Differential tests: the pure-R engines must produce byte-identical output to
# the original taxon-tools Docker image (camwebb/taxon-tools:v1.3.0). These are
# skipped unless docker (and the image) are available, so they do not run on
# machines without docker, but they are the authoritative fidelity check.

tt_image <- "camwebb/taxon-tools:v1.3.0"

# Run a taxon-tools command in the image, mounting `dir` at /data.
docker_tt <- function(dir, args) {
  system2(
    "docker",
    c("run", "--rm", "-v", paste0(dir, ":/data"), tt_image, args),
    stdout = TRUE, stderr = FALSE
  )
}

skip_if_no_image <- function() {
  skip_if_no_docker()
  ok <- tryCatch(
    system2(
      "docker", c("image", "inspect", tt_image),
      stdout = FALSE, stderr = FALSE
    ) == 0,
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) testthat::skip(paste("docker image", tt_image, "not present"))
}

# Build a sizeable, varied set of names from the bundled taxonomy plus
# deliberate perturbations (ranks, hybrids, basionym/ex/in authors, diacritics).
make_name_set <- function() {
  data(filmy_taxonomy, package = "taxastand", envir = environment())
  base <- unique(filmy_taxonomy$scientificName) # nolint: object_usage_linter.
  base <- base[!is.na(base) & nzchar(base)]
  ranks <- c("var.", "subsp.", "f.", "fo.", "forma", "nothovar.")
  perturb <- unlist(lapply(utils::head(base, 200), function(nm) {
    parts <- strsplit(nm, " ")[[1]]
    if (length(parts) < 2) {
      return(NULL)
    }
    g <- parts[1]
    s <- parts[2]
    c(
      paste0(g, " ", s, " ", ranks[1], " ", s),
      paste0("x ", g, " ", s),
      paste0(g, " x ", s),
      paste0(g, " ", s, " (Bl.) K. Iwats."),
      paste0(g, " ", s, " Smith ex Jones"),
      paste0(g, "ë ", s, " Müller")
    )
  }))
  unique(c(base, perturb))
}

test_that("tt_parsenames matches docker parsenames byte-for-byte", {
  skip_if_no_image()
  dir <- file.path(tempdir(), paste0("ttdiff_", as.integer(stats::runif(1, 0, 1e9))))
  dir.create(dir)
  names_vec <- make_name_set()
  records <- paste0("n", seq_along(names_vec), "|", names_vec)
  writeLines(records, file.path(dir, "in.txt"), useBytes = TRUE)

  docker_out <- docker_tt(dir, c("parsenames", "/data/in.txt"))
  r_out <- tt_parsenames(records)

  expect_identical(r_out, docker_out)
})

test_that("tt_matchnames matches docker matchnames -F across options", {
  skip_if_no_image()
  dir <- file.path(tempdir(), paste0("ttdiff_", as.integer(stats::runif(1, 0, 1e9))))
  dir.create(dir)

  names_vec <- make_name_set()
  # reference: parsed taxonomy names (drop unparseable + duplicate exact keys)
  drop_failed <- function(lines) {
    rest <- sub("^[^|]*\\|", "", lines)
    lines[gsub("\\|", "", rest) != ""]
  }
  ref_lines <- drop_failed(
    tt_parsenames(paste0("r", seq_along(names_vec), "|", names_vec))
  )
  ref_lines <- ref_lines[!duplicated(.tt_record_keys(ref_lines)$exact)]

  # query: misspellings + dropped authors of a sample, to exercise fuzzy/noauth
  set.seed(123)
  samp <- sample(names_vec, 250)
  mis <- vapply(samp, function(s) {
    cs <- strsplit(s, "")[[1]]
    pos <- grep("[a-z]", cs)
    if (length(pos) > 3) cs[pos[length(pos) %/% 2]] <- "x"
    paste(cs, collapse = "")
  }, character(1))
  noauth <- sub(" [A-Z(].*$", "", samp)
  query_names <- unique(c(samp, mis, noauth, "Totally unknown thing"))
  query_lines <- drop_failed(
    tt_parsenames(paste0("q", seq_along(query_names), "|", query_names))
  )
  query_lines <- query_lines[!duplicated(.tt_record_keys(query_lines)$exact)]

  writeLines(ref_lines, file.path(dir, "ref.txt"), useBytes = TRUE)
  writeLines(query_lines, file.path(dir, "query.txt"), useBytes = TRUE)

  combos <- list(
    list(e = 10, o1 = FALSE, oc = FALSE),
    list(e = 5, o1 = FALSE, oc = FALSE),
    list(e = 10, o1 = TRUE, oc = FALSE),
    list(e = 10, o1 = FALSE, oc = TRUE),
    list(e = 10, o1 = TRUE, oc = TRUE)
  )

  for (cb in combos) {
    args <- c(
      "matchnames", "-a", "/data/query.txt", "-b", "/data/ref.txt",
      "-o", "/data/out.txt", "-e", cb$e, "-F"
    )
    if (cb$o1) args <- c(args, "-1")
    if (cb$oc) args <- c(args, "-c")
    docker_tt(dir, args)
    docker_out <- readLines(file.path(dir, "out.txt"))

    r_out <- tt_matchnames(
      query_lines, ref_lines,
      max_dist = cb$e, match_no_auth = cb$o1, match_canon = cb$oc
    )

    # Output line order differs (docker streams per input line); compare as sets.
    expect_setequal(r_out, docker_out)
    expect_identical(length(r_out), length(docker_out))
  }
})
