#' Bulk download budget documents
#'
#' Download the following raw budget documents in Excel format (.xlsx)
#' from the Department of Budget and Management (DBM) website:
#' * National Expenditure Program (NEP)
#' * General Appropriations Act (GAA)
#'
#' @param type A character string or vector specifying the type of budget document(s) to download:
#'   * `"gaa"`; General Appropriations Act
#'   * `"nep"`; National Expenditure Program
#'   * `"all"` (the default); equivalent to `c("gaa", "nep")`
#'
#' @param year A list (or coercible to one) specifying the fiscal year(s) to download.
#'   The default value is `"all"` the years available.
#'   If more than one `type` is set, a single `year` value or vector will be recycled.
#'
#' @returns Invisibly returns `NULL` (called for side effects).
#' @export
#' @examples
#' \dontrun{
#' get_docs() # downloads all GAA and NEP Excel files from all the years available
#'
#' # equivalent calls----
#' get_docs("all", 2020:2024) # the year value will be recycled for each type
#' get_docs(c("gaa", "nep"), list(2020:2024, 2020:2024))
#' }
#' @seealso
#'   [get_gaa()] to download GAA documents
#'   [get_nep()] to download NEP documents
get_docs <- function(type = "all", year = "all") {
  if (!curl::has_internet()) stop("No internet connection")
  if (type == "all") {
    type <- c("gaa", "nep")
  }
  if (year == "all") {
    year <- lapply(type, function(type) {
      2020:get_recent_yr(type)
    })
  }
  if (!is.list(year)) {
    year <- as.list(year)
  }
  if (length(year) == 1) {
    year <- rep(year, length(type))
  }
  if (length(type) != length(year)) {
    stop("type and year must have the same length")
  }
  names(year) <- type
  download_docs(type, year)
}

#' Download GAA documents
#'
#' A wrapper for `get_docs(type = "gaa")`; this function downloads the
#' General Appropriations Act (GAA) Excel files (.xlsx) from the
#' Department of Budget and Management (DBM) website
#'
#' @inheritParams get_docs
#'
#' @returns `NULL`
#'
#' @export
#' @examples
#' \dontrun{
#' get_gaa() # Downloads all GAA files from 2020 onwards
#' }
get_gaa <- function(year = "all") {
  get_docs("gaa", year)
}

#' Download NEP documents
#'
#' #' A wrapper for `get_docs(type = "nep")`; this function downloads the
#' General Appropriations Act (GAA) Excel files (.xlsx) from the
#' Department of Budget and Management (DBM) website
#'
#' @inheritParams get_docs
#'
#' @returns `NULL`
#'
#' @export
#' @examples
#' \dontrun{
#' get_nep() # Downloads all NEP files from 2020 onwards
#' }
get_nep <- function(year = "all") {
  get_docs("nep", year)
}
