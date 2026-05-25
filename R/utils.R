# look for the most recent fiscal year a document is available
check_recent_year <- function(doc_type) {
  # fetch budget pages
  pages <- get_budget_pages(doc_type)
  year <- gsub(".*/(\\d{4})/.*", "\\1", pages) |> 
    as.numeric() |> 
    max()
  return(year)
}


download_docs <- function(type, year) {
  download_links <- get_download_link(type, year)
  destfiles <- generate_destfiles(download_links)
  mapply(\(link, destfile) download.file(link, destfile),
  download_links, destfiles)
}

get_download_link <- function(doc_type, year) {
  pages <- get_budget_pages(doc_type, year)
  pattern <- "xlsx?"
  links <- pages |> vapply(\(page) search_link(page, pattern, year))
  return(links)
}

generate_destfiles <- function(download_links) {
  base_dir <- tools::R_user_dir("ropenbudgetph", which = "data")
  directories <- paste0(base_dir, c("gaa", "nep"), "/")
  
  invisible(lapply(directories, \(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
}))
  
  destfiles <- ifelse(
  grepl("GAA", dl_links),
  paste0(directories[1], basename(dl_links)),
  paste0(directories[2], basename(dl_links))
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

  if(!is.null(year)) # filter for year if not null
    link <- grepv(pattern = paste(year, collapse = "|"), x = link)
  
  link <- grepv(pattern = pattern, x = link) |> unique()

  return(link)
}

# custom fetching of html with caching in mind
fetch_html <- function(url) {
  cache_dir <- tools::R_user_dir("ropenbudgetph", which = "cache")
  if (!dir.exists(cache_dir))
    dir.create(cache_dir, recursive = TRUE)
  filename <- paste0(gsub("[^a-zA-Z0-9]", "_", url), ".rds")
  cached_file <- file.path(cache_dir, filename)

  cached_obj <- if (file.exists(cached_file)) readRDS(cached_file) else NULL

  req <- httr2::request(url) #|>
    # httr2::req_user_agent("ropenbudgetph/0.9000")

  # add cache-related http headers to request object
  if (!is.null(cached_obj) && !is.null(cached_obj$headers)) {
    headers_list <- list()
    if (!is.null(cached_obj$headers$etag))
      headers_list[["if-none-match"]] <- cached_obj$headers$etag
    if (!is.null(cached_obj$headers$`last-modified`))
      headers_list[["if-modified-since"]] <- cached_obj$headers$`last-modified`
    if(length(headers_list) > 0)
      req <- do.call(httr2::req_headers, c(list(req), headers_list))
  }

  resp <- tryCatch(httr2::req_perform(req), error = function(e) e)

  if (inherits(resp, "error")) {
    if (!is.null(cached_obj)) return(cached_obj$body)
    stop(resp)
  }

  status <- httr2::resp_status(resp)

  # if not modified
  if (status == 304 && !is.null(cached_obj)) {
    return(cached_obj$body)
  }

  if (status >= 200 && status < 300) {
    resp_body <- httr2::resp_body_html(resp)
    resp_hdrs <- httr2::resp_headers(resp)
    names(resp_hdrs) <- tolower(names(resp_hdrs))
    saveRDS(list(body = resp_body, headers = resp_hdrs, fetched_at = Sys.time()), cached_file)
    return(resp_body)
  }

  if (!is.null(cached_obj)) return(cached_obj$body)

  stop("Failed to fetch and no cache available: ", url)
}
