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

get_download_links <- function(doc_type, year = NULL) {
  pages <- get_budget_pages(doc_type, year)

  links <- vapply(
    pages,
    function(page) {
      links_per_page <- get_links(page)
      target_link <- grepv("\\.xlsx?$", links_per_page)[1]
      return(target_link)
    },
    character(length(pages))
  )

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
  budget_main <- paste0(DBM_URL, "/index.php/budget")
  budget_fys <- get_links(budget_main)

  pattern <- gsub(" ", "-", DOC_TYPES[doc_type]) |> tolower()
  pattern <- paste0("(?<=/\\d{4}/)(", pattern, ")", collapse = "|")
  budget_fys <- grepv(pattern, budget_fys, perl = TRUE) |> unique()

  # filter by year if not null
  if (!is.null(year)) {
    budget_fys <- grepv(paste(year, collapse = "|"), budget_fys)
  }

  return(budget_fys)
}

# returns all links from a page
get_links <- function(page_url) {
  links <- xml2::read_html(fetch_html(page_url)) |>
    xml2::xml_find_all("//a") |>
    xml2::xml_attr("href") |>
    xml2::url_absolute(DBM_URL)

  return(links)
}
