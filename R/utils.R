# look for the most recent fiscal year a document is available
check_recent_year <- function(doc_type) {
  if (!curl::has_internet()) {
    stop("No internet connection")
  }
  pages <- get_budget_pages(doc_type)
  year <- gsub(".*/(\\d{4})/.*", "\\1", pages) |>
    as.numeric() |>
    max()
  return(year)
}

download_docs <- function(type, year) {
  download_links <- get_download_link(type, year)
  destfiles <- generate_destfiles(download_links)
  mapply(
    \(link, destfile) download.file(link, destfile),
    download_links,
    destfiles
  )
}

get_download_link <- function(doc_type, year) {
  pages <- get_budget_pages(doc_type, year)
  pattern <- "xlsx?"
  links <- pages |> vapply(\(page) search_link(page, pattern, year))
  return(links)
}

generate_destfiles <- function(download_links) {
  base_dir <- tools::R_user_dir("ropenbudgetph", "data")
  directories <- paste0(base_dir, c("gaa", "nep"), "/")

  invisible(lapply(directories, \(directory) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }))

  destfiles <- ifelse(
    grepl("GAA", download_links),
    paste0(directories[1], basename(download_links)),
    paste0(directories[2], basename(download_links))
  )
}

get_budget_pages <- function(doc_type, year = NULL) {
  base_url <- "https://www.dbm.gov.ph"
  budget_page <- paste0(base_url, "/index.php/budget")
  pattern <- generate_pattern(doc_type)
  pages <- search_link(budget_page, pattern, year)
  return(pages)
}

generate_pattern <- function(doc_type) {
  patterns <- c(
    gaa = "/general-appropriations",
    nep = "\\d/national-expenditure"
  )
  return(paste(patterns[doc_type], collapse = "|"))
}

# returns all link if year = null
search_link <- function(page_url, pattern, year = NULL) {
  link <- fetch_html(page_url) |>
    xml2::xml_find_all("//a") |>
    xml2::xml_attr("href")

  if (!is.null(year)) {
    # filter for year if not null
    link <- grepv(pattern = paste(year, collapse = "|"), x = link)
  }

  link <- grepv(pattern = pattern, x = link) |> unique()

  return(link)
}

# custom fetching of html file with caching in mind
# This is a workaround for DBM server's no-store and no-cache Cache Control directives
# httr2::req_cache don't work
fetch_html <- function(url) {
  cache_dir <- file.path(tools::R_user_dir("ropenbudgetph", "cache"))
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE)
  }
  cache_file <- paste0(rlang::hash(url), ".rds")
  cache_path <- file.path(cache_dir, cache_file)
  cache_obj <- if (file.exists(cache_path)) readRDS(cache_path) else NULL

  if (!is.null(cache_obj)) {
    cache_time <- cache_obj$dt
    cache_age <- difftime(Sys.time(), cache_time, units = "days")
    if (cache_age < 1) return(cache_obj$body)
  }

  if (!curl::has_internet()) {
    warning("Offline: attempting cache-only fetch")
    if (!is.null(cache_obj)) {
      return(cache_obj$body)
    }
    stop("Offline and no cache available")
  }

  req <- httr2::request(url)
  resp <- tryCatch(
    httr2::req_perform(req),
    error = function(e) {
      if (!is.null(cache_obj)) {
        warning("Request error: ", e$message, ". Using stale cache.")
        return(cache_obj$body)
      }
      stop("Request failed: ", e$message)
    }
  )

  status <- httr2::resp_status(resp)
  if (status >= 200 && status < 300) {
    resp_body <- httr2::resp_body_html(resp)
    tryCatch(
      saveRDS(list(body = resp_body, dt = Sys.time()), cache_path),
      error = function(e) warning("Failed to update cache: ", e$message)
    )
    return(resp_body)
  }

  if (!is.null(cache_obj)) {
    warning("HTTP ", status, " for ", url, ". Using stale cache.")
    return(cache_obj$body)
  }
  stop("HTTP ", status, " for ", url, " and no cache available")
}
