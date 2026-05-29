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
  directories <- file.path(base_dir, c("gaa", "nep"))

  invisible(lapply(directories, \(directory) {
    if (!dir.exists(directory)) {
      dir.create(directory, recursive = TRUE)
    }
  }))

  destfiles <- ifelse(
    grepl("GAA", download_links),
    file.path(directories[1], basename(download_links)),
    file.path(directories[2], basename(download_links))
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
  link <- xml2::read_html(fetch_html(page_url)) |>
    xml2::xml_find_all("//a") |>
    xml2::xml_attr("href")

  if (!is.null(year)) {
    # filter for year if not null
    link <- grepv(pattern = paste(year, collapse = "|"), x = link)
  }

  link <- grepv(pattern = pattern, x = link) |> unique()

  return(link)
}
