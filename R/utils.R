DBM_URL <- "https://www.dbm.gov.ph"

the <- new.env(parent = emptyenv())
the$DOC_TYPES <- NULL
the$FISCAL_YEARS <- NULL

# for mocking purposes
has_connection <- function() {
  curl::has_internet()
}

load_dt <- function() {
  the$DOC_TYPES <- c(
    gaa = "General Appropriations Act",
    nep = "National Expenditure Program"
  )
}

# assigns a list of budget document type and fiscal years supported for download, to FISCAL_YEARS object
load_fys <- function() {
  if(is.null(the$DOC_TYPES)) load_dt()
  pages <- get_budget_pages(names(the$DOC_TYPES))
  doc_fy <- lapply(the$DOC_TYPES, function(dt) {
    pages[agrep(dt, basename(pages), ignore.case = TRUE)] |>
      gsub(".*/(\\d{4})/.*", "\\1", x = _) |>
      as.numeric() |>
      sort()
  })
  the$FISCAL_YEARS <- doc_fy
}

download_docs <- function(type, year) {
  links <- get_download_links(type, year)
  destfiles <- generate_destfiles(type, links)
  mapply(
    function(link, destfile) download.file(link, destfile),
    links,
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

generate_destfiles <- function(type, link) {
  base_dir <- tools::R_user_dir("ropenbudgetph", "data")
  dirs <- file.path(base_dir, type)
  names(dirs) <- type

  invisible(lapply(dirs, \(directory) {
    if (!dir.exists(directory)) dir.create(directory, recursive = TRUE)
  }))

  link_type <- sub(".*((?:GAA)|(?:NEP)).*", "\\1", basename(link)) |> tolower()
  destfiles <- file.path(dirs[link_type], basename(link))

  return(destfiles)
}

get_budget_pages <- function(doc_type, year = NULL) {
  budget_main <- paste0(DBM_URL, "/index.php/budget")
  budget_fys <- get_links(budget_main)
  
  if(is.null(the$DOC_TYPES)) load_dt()
  pattern <- gsub(" ", "-", the$DOC_TYPES[doc_type]) |> tolower()
  pattern <- paste0("(?<=/\\d{4}/)(", pattern, ")", collapse = "|")
  budget_fys <- grepv(pattern, budget_fys, perl = TRUE) |> unique()

  # filter by year if specified
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
