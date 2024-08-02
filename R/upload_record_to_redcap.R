#' Uploads a record to REDCap using the REDCap API.
#'
#' @param api_url The URL of the REDCap API.
#' @param token The API token for authentication.
#' @param record_data A list containing the data for the record to be uploaded.
#' @param forceAutoNumber A logical value indicating whether to force auto-numbering for the record (default is TRUE).
#'
#' @return The JSON response from the API.
#'
#' @examples
#' # Example usage:
#' api_url <- "https://redcap.example.com/api/"
#' token <- "your_api_token"
#' record_data <- list(
#'   record_id = 1,
#'   field1 = "value1",
#'   field2 = "value2"
#' )
#' response <- upload_record_to_redcap(api_url, token, record_data)
#' print(response)
#'
#' @import httr
#' @importFrom jsonlite fromJSON
upload_record_to_redcap <- function(api_url, token, record_data, forceAutoNumber) {
  payload <- list(
    token = token,
    action = 'import',
    content = 'record',
    format = 'json',
    type = 'flat',
    overwriteBehavior = 'normal',
    forceAutoNumber = 'true',  # Default value
    dateFormat = 'YMD',
    data = record_data
  )
  
  # Update forceAutoNumber value if specified
  if (!forceAutoNumber) {
    payload[['forceAutoNumber']] <- 'false'
  }
  
  # Send POST request
  response <- httr::POST(url = api_url, body = payload, encode = 'form')
  
  if (response$status_code == 200) {
    # Parse JSON response
    json_data <- httr::content(response)
    return(json_data)
  } else {
    # Handle error
    error_message <- paste('HTTP Status', response$status_code, ':', rawToChar(response$content))
    stop(error_message)
    
  }
}
