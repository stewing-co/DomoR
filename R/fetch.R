#' Fetch a data source.
#'
#' Retrieves a data source by ID (GUID) or by
#' previous list() index from Domo,
#' converts it to a data.frame and returns.
#'
#' @param id A data source id (GUID) or an index from a previous list.
#' @param columns A vector of column names to return from the Domo datasource. (not case sensitive)
#' @param ... Additional httr options
#' @return A \code{data.frame} built from the requested Domo data source.
#' @export
#' @examples
#' DomoR::init(Sys.getenv('DOMO_BASE_URL'), Sys.getenv('DEVELOPER_TOKEN'))
#' df <- DomoR::fetch('4826e3fb-cd23-468d-9aff-96bf5b690247')
#' DomoR::list_ds(limit=10)
#' df <- DomoR::fetch(1)
#' df <- DomoR::fetch('4826e3fb-cd23-468d-9aff-96bf5b690247',
#'   c('accountid', 'lastname', 'startdate'),
#'   httr::progress())
# In fetch.R
# In fetch.R
fetch <- function(id, columns = NULL, use.make.names=FALSE, guessEncoding=TRUE, buffer_size = 5000000, ...) {

  if (!exists("customer.url", .domo_env)) {
    stop("Must call DomoR::init() first", call.=FALSE)
  }

  if (is.numeric(id)) {
    if (!exists("last_data_source_list", .domo_env)) stop("No list to index", call.=FALSE)
    data_source_id <- .domo_env$last_data_source_list[[id]]
  } else {
    data_source_id <- id
  }

  get_url <- paste0(.domo_env$customer.url, '/api/data/v2/datasources/',
                    data_source_id, '/dataversions/latest?includeHeader=true')

  all.headers <- httr::add_headers(c(.domo_env$auth.token, .domo_env$user.agent, 'Accept'='text/csv'))

  get_result <- httr::GET(get_url, all.headers, .domo_env$config, ...)
  httr::stop_for_status(get_result)

  raw_content <- httr::content(get_result, as = "raw")

  if(guessEncoding){
    guessed <- readr::guess_encoding(raw_content)
    encoding <- ifelse(is.null(guessed), "UTF-8", guessed$encoding[1])
    if (encoding == "ASCII") encoding <- "UTF-8"
  } else {
    encoding <- "UTF-8"
  }

  # Increase buffer size safely around read_csv
  old_buffer_size <- Sys.getenv("VROOM_CONNECTION_SIZE")
  on.exit(Sys.setenv(VROOM_CONNECTION_SIZE = old_buffer_size))
  Sys.setenv(VROOM_CONNECTION_SIZE = buffer_size)

  df <- readr::read_csv(raw_content, locale = readr::locale(encoding = encoding), show_col_types = FALSE)

  if (!is.null(columns)) {
    df <- df[,tolower(names(df)) %in% tolower(columns), drop=FALSE]
  }

  if(use.make.names){
    names(df) <- make.names(tolower(names(df)))
  }

  return(as.data.frame(df))
}


