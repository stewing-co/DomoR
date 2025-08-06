# Define package-level environment
.domo_env <- new.env(parent = emptyenv())

# On package load
.onLoad <- function(libname, pkgname) {
  packageStartupMessage("Welcome to DomoR")
}

#' Initialize DomoR Package
#'
#' Initializes the connection settings for the DomoR package.
#'
#' @param customer The customer ID or base URL (e.g., 'acme' or 'acme.domo.com').
#' @param token The developer API token from Domo.
#' @param config Optional httr configuration settings.
#' @param verbose Logical, enable verbose output.
#' @export
#' @examples
#' DomoR::init(Sys.getenv('DOMO_BASE_URL'), Sys.getenv('DEVELOPER_TOKEN'))
init <- function(customer,
                 token,
                 config = NULL,
                 verbose = FALSE) {

  # Check required packages
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("'httr' is required but not installed", call. = FALSE)
  }

  # Check plugin status (deprecated checks)
  get_pluginstatus_result <- httr::GET('https://s3.amazonaws.com/domoetl/get/R2.json')

  if (get_pluginstatus_result$status_code != 403) {
    httr::stop_for_status(get_pluginstatus_result)
    result_content <- httr::content(get_pluginstatus_result, as = "parsed", type = "application/json")

    if (result_content$deprecated[1]) {
      warning(result_content$message[1])
    }

    if (result_content$obsolete[1]) {
      stop(result_content$message[1])
    }
  }

  # Validate parameters
  if (missing(customer) || !is.character(customer)) {
    stop('A valid customer instance is required')
  }

  if (missing(token)) {
    stop('A valid token is required')
  }

  # Set values in package environment
  .domo_env$customer <- customer
  .domo_env$customer.url <- paste0("https://", with.suffix(customer))

  .domo_env$auth.token <- if (nchar(token) < 70) {
    c('X-DOMO-Developer-Token' = token)
  } else {
    c('X-DOMO-Authentication' = token)
  }

  .domo_env$user.agent <- c("User-Agent" = "DomoR-test/1.0")

  .domo_env$config <- if (is.null(config)) {
    if (verbose) httr::verbose() else httr::config()
  } else {
    config
  }
}

#' Append a suffix to a customer name if missing
#'
#' Ensures the provided customer name includes the '.domo.com' suffix.
#'
#' @param customer The customer name or base URL.
#' @param suffix The suffix to append (default '.domo.com').
#' @return A character string with the suffix appended if necessary.
#' @export
with.suffix <- function(customer, suffix = '.domo.com') {
  customer <- tolower(customer)
  if (grepl(paste0(suffix, "$"), customer, ignore.case = TRUE)) {
    customer
  } else {
    paste0(customer, suffix)
  }
}
