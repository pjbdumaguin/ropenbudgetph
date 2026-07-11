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
#'   * `NULL` (the default); equivalent to `c("gaa", "nep")`
#'
#' @param year A list (or coercible to one) specifying the fiscal year(s) to download.
#'   The default value is `NULL` or all the years supported for download. (currently 2020 up to the most recent fiscal year)
#'   If more than one `type` is set, a single `year` value or vector will be recycled.
#'
#' @returns Invisibly returns `NULL` (called for side effects).
#' @export
#' @examples
#' \dontrun{
#' get_docs() # downloads all GAA and NEP Excel files from all the years available
#'
#' # equivalent calls----
#' get_docs(2020:2024) # the year value will be recycled for each type
#' get_docs(c("gaa", "nep"), list(2020:2024, 2020:2024))
#' }
#' @seealso
#'   [get_gaa()] to download GAA documents
#'   [get_nep()] to download NEP documents
get_docs <- function(type = NULL, year = NULL) {
  check_internet()
  args <- normalize_args(type, year)
  # download_docs(args$type, args$year)
  print(args)
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
get_gaa <- function(year = NULL) {
  get_docs("gaa", year)
}

#' Download NEP documents
#'
#' A wrapper for `get_docs(type = "nep")`; this function downloads the
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
get_nep <- function(year = NULL) {
  get_docs("nep", year)
}

assert_type <- function(type, supported) {
  if (is.null(type)) stop("`type` cannot be NULL")
  if (!is.character(type)) stop("`type` must be character, not ", typeof(type))
  if (!length(type)) stop("`type` must not be empty")

  bad <- type %notin% names(supported)
  if (any(bad)) stop("invalid budget document: ", toString(type[bad]))

  if (anyDuplicated(type)) {
    warning("`type` duplicates found; repeats removed")
    type <- unique(type)
  }

  type
}

assert_year <- function(year, type, fys) {
  if (is.null(year)) stop("`year` cannot be NULL")
  if (!(is.numeric(year) || (is.list(year) && all(vapply(year, is.numeric, logical(1)))))) {
    stop("`year` must be numeric or a list of numeric vectors")
  }

  if (!is.list(year)) year <- list(year)
  if (!length(year)) stop("`year` must not be empty")
  if (length(year) > length(type)) stop("`year` length cannot exceed `type` length")

  yr <- unique(unlist(year, use.names = FALSE))
  supported_yr <- unique(unlist(fys[type], use.names = FALSE))
  bad <- yr %notin% supported_yr
  if (any(bad)) stop("`year` unsupported: ", toString(yr[bad]))

  if (length(year) < length(type)) {
    if (length(type) %% length(year) != 0) {
      warning(sprintf(
        "Length of `type` (%i) is not a multiple of length `year` (%i)",
        length(type), length(year)
      ))
    }
    year <- rep(year, length.out = length(type))
  }

  if (is.null(names(year))) names(year) <- type
  year
}

normalize_args <- function(type = NULL, year = NULL, supported = get_doc_ls(), fys = get_fys()) {
  if (is.null(type)) type <- names(supported)
  if (is.list(year) && !is.null(names(year))) type <- names(year)

  type <- assert_type(type, supported)

  if (is.null(year)) year <- fys[type]
  year <- assert_year(year, type, fys)

  list(type = type, year = year)
}

