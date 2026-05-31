DBM_URL <- "https://www.dbm.gov.ph"

DOC_TYPES <- c(
  gaa = "General Appropriations Act",
  nep = "National Expenditure Program"
)

# TODO vectorize the result
get_recent_yr <- function(doc_type) {
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

get_download_link <- function(doc_type, year = NULL) {
  pages <- get_budget_pages(doc_type, year)
  pattern <- "xlsx?"
  links <- lapply(pages, \(page) search_link(page, pattern, year)[1])
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
  budget_page <- paste0(DBM_URL, "/index.php/budget")
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
  links <- xml2::read_html(fetch_html(page_url)) |>
    xml2::xml_find_all("//a") |>
    xml2::xml_attr("href") |>
    xml2::url_absolute(DBM_URL)

  # filter year if not null
  if (!is.null(year)) {
    links <- grepv(pattern = paste(year, collapse = "|"), x = links)
  }

  links <- grepv(pattern = pattern, x = links) |> unique()

  return(links)
}
