# Enrich the Austraits source bibliography with abstracts from OpenAlex.
#
# Run sections from the editor with the project root as working directory
# (not via Rscript/source() end-to-end): the DOI-batched pass and the
# per-entry title-search pass are meant to be run and inspected separately,
# since the title pass is slow and its matches need more scrutiny.
#
# Requires an OpenAlex API key to raise the rate limit from 10 to 30 req/s and
# avoid backoff.
#
# Outputs (does not modify the original Austraits data):
#   - data/sources/austraits_sources_with_abstracts.bib: full bibliography,
#     BibLaTeX format, with an `abstract` field added where a candidate was
#     verified (title, year, and DOI/first author all agree).
#   - data/sources/openalex_matches.csv: one row per source entry recording
#     the match method and status, for auditing before the .bib is used.
#     status values: already_has_abstract, no_identifier_or_title, enriched,
#     no_openalex_abstract, not_found, review_doi_metadata,
#     review_title_candidates, request_failed.

library(httr2)
library(RefManageR)
library(austraits)

`%||%` <- function(x, y) if (is.null(x)) y else x

output <- "data/sources/austraits_sources_with_abstracts.bib"
log_file <- "data/sources/openalex_matches.csv"

austraits <- load_austraits(version = "7.0.0", update = FALSE)
sources <- austraits$sources
stopifnot(inherits(sources, "BibEntry"), !anyDuplicated(names(sources)))

# Strip braces/diacritics/punctuation so titles from BibTeX and OpenAlex
# compare equal regardless of LaTeX escaping or accented characters.
normalize_title <- function(x) {
  x <- gsub("[{}]", "", x)
  x <- iconv(x, to = "ASCII//TRANSLIT", sub = "")
  gsub("[^a-z0-9]", "", tolower(x))
}

# Source DOIs are bare (e.g. "10.1071/..."); OpenAlex returns full URLs.
normalize_doi <- function(x) {
  tolower(sub(
    "^https?://(dx\\.)?doi\\.org/",
    "",
    trimws(x),
    ignore.case = TRUE
  ))
}

# OpenAlex never returns a plaintext abstract (licensing); it returns a
# word -> position-list index instead. Reassemble the plaintext from it.
abstract_text <- function(index) {
  if (is.null(index) || !length(index)) {
    return(NULL)
  }
  positions <- unlist(index, use.names = FALSE)
  words <- rep(names(index), lengths(index))
  paste(words[order(positions)], collapse = " ")
}

fetch_works <- function(query) {
  req <- request("https://api.openalex.org/works") |>
    req_url_query(
      !!!query,
      select = "id,doi,title,publication_year,authorships,abstract_inverted_index"
    ) |>
    req_retry(max_tries = 4, is_transient = function(resp) {
      resp_status(resp) %in% c(429, 500, 502, 503, 504)
    })
  api_key <- Sys.getenv("openalexR.apikey")
  if (nzchar(api_key)) {
    req <- req |> req_url_query(api_key = api_key)
  }
  resp_body_json(req_perform(req), simplifyVector = FALSE)$results
}

# OpenAlex allows up to 100 DOIs OR-joined in one `filter`, so this turns up
# to 383 source entries into ~4 requests instead of 383.
fetch_works_by_doi <- function(dois) {
  chunks <- split(dois, ceiling(seq_along(dois) / 100))
  works <- lapply(chunks, function(chunk) {
    fetch_works(list(
      filter = paste0(
        "doi:",
        paste0("https://doi.org/", chunk, collapse = "|")
      ),
      per_page = 100,
      corpus = "all"
    ))
  })
  unlist(works, recursive = FALSE)
}

# A candidate from OpenAlex is only accepted if title and year agree with
# the source entry, and (for DOI matches) the DOI also agrees, or (for
# title-search matches, which have no DOI to check) the first author's
# family name appears in the candidate's first author's display name.
match_work <- function(entry, work, method) {
  if (is.null(work$title) || is.null(work$publication_year)) {
    return(FALSE)
  }
  entry_title <- entry[[1]]$title
  entry_year <- entry[[1]]$year
  if (is.null(entry_title) || is.null(entry_year)) {
    return(FALSE)
  }
  if (!identical(normalize_title(entry_title), normalize_title(work$title))) {
    return(FALSE)
  }
  if (!identical(as.integer(entry_year), as.integer(work$publication_year))) {
    return(FALSE)
  }
  if (method == "doi") {
    return(
      !is.null(work$doi) &&
        identical(normalize_doi(entry[[1]]$doi), normalize_doi(work$doi))
    )
  }
  if (!length(work$authorships) || !length(entry[[1]]$author)) {
    return(FALSE)
  }
  first_author <- work$authorships[[1]]$author$display_name
  source_author <- entry[[1]]$author[[1]]$family
  # family can be multi-word (e.g. c("van", "der", "Berg")); any() keeps the
  # result length 1 for `&&`.
  !is.null(first_author) &&
    !is.null(source_author) &&
    any(
      normalize_title(source_author) %in%
        strsplit(tolower(gsub("[^[:alnum:] ]", "", first_author)), " +")[[1]]
    )
}

# Run after reviewing the match rules above; batches DOI lookups, then
# queries OpenAlex per entry for the remaining title-only sources.
keys <- names(sources)
log <- data.frame(
  key = keys,
  method = NA_character_,
  status = "not_checked",
  openalex_id = NA_character_,
  candidate_title = NA_character_,
  candidate_year = NA_integer_,
  candidate_doi = NA_character_,
  stringsAsFactors = FALSE
)

# Scores a set of OpenAlex candidates for one source entry against
# `match_work()` and records the outcome in `log` and, on success, writes
# the abstract back into `sources` (both modified in the caller's
# environment via `<<-`, since this runs inside a loop over `i`).
record_match <- function(i, entry, works, method) {
  if (!length(works)) {
    log$status[i] <<- "not_found"
    return(invisible())
  }
  verified <- which(vapply(
    works,
    function(w) match_work(entry, w, method),
    logical(1)
  ))
  if (length(verified) != 1L) {
    log$status[i] <<- if (method == "title") {
      "review_title_candidates"
    } else {
      "review_doi_metadata"
    }
    candidate <- works[[1]]
  } else {
    candidate <- works[[verified]]
    abstract <- abstract_text(candidate$abstract_inverted_index)
    if (is.null(abstract)) {
      log$status[i] <<- "no_openalex_abstract"
    } else {
      sources[[i]][[1]]$abstract <<- abstract
      log$status[i] <<- "enriched"
    }
  }
  log$openalex_id[i] <<- candidate$id %||% NA_character_
  log$candidate_title[i] <<- candidate$title %||% NA_character_
  log$candidate_year[i] <<- candidate$publication_year %||% NA_integer_
  log$candidate_doi[i] <<- candidate$doi %||% NA_character_
}

# Pass 1: decide per entry whether it already has an abstract, has a DOI
# (preferred — exact match), has only a title to search on, or has neither.
for (i in seq_along(sources)) {
  entry <- sources[[i]]
  if (!is.null(entry[[1]]$abstract) && nzchar(entry[[1]]$abstract)) {
    log$status[i] <- "already_has_abstract"
    next
  }
  doi <- entry[[1]]$doi
  log$method[i] <- if (!is.null(doi) && nzchar(doi)) {
    "doi"
  } else if (!is.null(entry[[1]]$title)) {
    "title"
  } else {
    NA_character_
  }
}
log$status[
  is.na(log$method) & log$status == "not_checked"
] <- "no_identifier_or_title"

# Pass 2: DOI entries, fetched in batches of up to 100 (see
# fetch_works_by_doi), then matched back to their source row by DOI.
doi_idx <- which(log$method == "doi")
doi_lookup <- normalize_doi(vapply(
  sources[doi_idx],
  function(e) e[[1]]$doi,
  character(1)
))
works_by_doi <- tryCatch(fetch_works_by_doi(doi_lookup), error = function(e) e)
if (inherits(works_by_doi, "error")) {
  log$status[doi_idx] <- "request_failed"
  warning(conditionMessage(works_by_doi), call. = FALSE)
} else {
  work_doi <- normalize_doi(vapply(
    works_by_doi,
    function(w) w$doi %||% NA_character_,
    character(1)
  ))
  pb <- txtProgressBar(min = 0, max = length(doi_idx), style = 3)
  for (j in seq_along(doi_idx)) {
    setTxtProgressBar(pb, j)
    i <- doi_idx[j]
    record_match(
      i,
      sources[[i]],
      works_by_doi[work_doi == doi_lookup[j]],
      "doi"
    )
  }
  close(pb)
}

# Pass 3: title-only entries, one OpenAlex search per entry (no batched
# title search exists). This is the slow part and worth running separately
# so `log` can be inspected before the final write.
title_idx <- which(log$method == "title")
pb <- txtProgressBar(min = 0, max = length(title_idx), style = 3)
for (j in seq_along(title_idx)) {
  setTxtProgressBar(pb, j)
  i <- title_idx[j]
  title <- sources[[i]][[1]]$title
  result <- tryCatch(
    fetch_works(list(
      `search.title` = gsub("[{}]", "", title),
      per_page = 25,
      corpus = "all"
    )),
    error = function(e) e
  )
  if (inherits(result, "error")) {
    log$status[i] <- "request_failed"
    warning(keys[i], ": ", conditionMessage(result), call. = FALSE)
    next
  }
  record_match(i, sources[[i]], result, "title")
}
close(pb)

# Biblatex format retains abstracts; RefManageR's Bibtex format omits them.
RefManageR::WriteBib(sources, file = output, biblatex = TRUE, verbose = FALSE)
utils::write.csv(log, log_file, row.names = FALSE, na = "")
stopifnot(length(sources) == length(keys), identical(names(sources), keys))
table(log$status)
