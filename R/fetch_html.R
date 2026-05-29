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
    if (cache_age < 1) {
      return(cache_obj$body)
    }
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
    resp_body <- httr2::resp_body_string(resp)
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