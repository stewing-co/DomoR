#' Fetch a data source to disk.
#'
#' Retrieves a data source by GUID, saves to temp file and converts it to a data.frame and returns.
#'
#' @param data_source_id A data source id (GUID)
#' @param nrows number of rows to return in dataframe by default returns all rows
#' @param delete.tmp.file temp file to save the data by default tmp_file is deleted
#' @return A \code{data.frame} built from the requested Domo data source
#' @export
#' @examples
#' DomoR::init(Sys.getenv('DOMO_BASE_URL'), Sys.getenv('DEVELOPER_TOKEN'))
#' df <- DomoR::fetch_to_disk(data_source_id="4826e3fb-cd23-468d-9aff-96bf5b690247", nrows=5, delete.tmp.file=TRUE)
# In fetch_dataset_disk.R
# In fetch_dataset_disk.R
fetch_to_disk <- function(data_source_id, nrows = NULL, delete.tmp.file = TRUE) {
  if (!exists("customer", .domo_env) || !exists("auth.token", .domo_env)) {
    stop("Must call DomoR::init() before fetch_to_disk()", call. = FALSE)
  }

  url <- paste0(.domo_env$customer.url, "/api/data/v2/datasources/", data_source_id, "/dataversions/latest?includeHeader=true")
  tmp <- tempfile(fileext = ".csv")
  resp <- httr::GET(url, httr::add_headers(c(.domo_env$auth.token, .domo_env$user.agent)), .domo_env$config,
                    httr::write_disk(tmp, overwrite = TRUE))
  httr::stop_for_status(resp)

  g <- readr::guess_encoding(tmp)
  enc <- if (is.null(g)) "UTF-8" else g$encoding[1]
  if (enc == "ASCII") enc <- "UTF-8"

  df <- if (is.null(nrows)) {
    readr::read_csv(tmp, locale = readr::locale(encoding = enc), show_col_types = FALSE)
  } else {
    readr::read_csv(tmp, locale = readr::locale(encoding = enc), n_max = nrows, show_col_types = FALSE)
  }

  if (delete.tmp.file) file.remove(tmp)
  as.data.frame(df)
}

