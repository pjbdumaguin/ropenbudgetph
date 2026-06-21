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
  if (!has_connection()) {
    stop("internet connection not detected\n\tplease connect and try again")
  }

  type <- type %||% names(get_doc_ls())
  # if `year` is a named list, overwrite the `type`
  if (is.list(year) && !is.null(names(year))) {
    type <- names(year)
  }

  if (!is.character(type)) {
    stop("`type` must be character, not ", typeof(type))
  }

  if (length(type) <= 0 || length(type) > length(get_doc_ls())) {
    stop(
      "`type` length, invalid: ",
      length(type),
      "\ninput `type` length can't be longer than currently supported: ",
      length(get_doc_ls())
    )
  }

  type_invalid <- type %notin% names(get_doc_ls())
  if (any(type_invalid)) {
    stop(
      "invalid budget document: ",
      toString(type[type_invalid], width = 12),
      "\ncurrently supported: ",
      paste0('"', names(get_doc_ls()), '"', collapse = ", ")
    )
  }

  if (anyDuplicated(type)) {
    is_dup <- duplicated(type)
    if (!(is.list(year) && is.null(names(year)))) {
      year <- year[!is_dup]
    }
    type <- unique(type)
    warning("`type` duplicates found; repeats removed")
  }

  year <- year %||% get_fys()[type]

  if (!is.numeric(year)) {
    if (!is.list(year) || !all(vapply(year, is.numeric, logical(1)))) {
      stop("`year` must be numeric or a list of numeric vectors")
    }
  }

  if (!is.list(year)) {
    year <- list(year)
  }

  if (length(year) <= 0 || length(year) > length(type)) {
    stop(
      "`year` length, invalid: ",
      length(year),
      "\nthere should only be one (set of) `year`(s) per input `type`"
    )
  }

  input_yr <- unique(unlist(year))
  supported_yr <- unique(unlist(get_fys()[type]))
  if (any(input_yr %notin% supported_yr)) {
    stop(
      "`year` unsupported: ",
      toString(input_yr[input_yr %notin% supported_yr], width = 16)
    )
  }

  if (length(year) < length(type)) {
    if (length(type) %% length(year) != 0) {
      warning(sprintf(
        "Length of `type` (%i) is not a multiple of length `year` (%i)",
        length(type),
        length(year)
      ))
    }
    year <- rep(year, length.out = length(type))
  }

  if (is.null(names(year))) {
    names(year) <- type
  }
  # download_docs(type, year)
  print(year)
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
