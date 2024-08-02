# Test connection
#
# This function tests the connection to an API endpoint by sending a POST request with the provided token and API URL.
# It returns the response content if the connection is successful, and prints the project details if not silent.
#
# Parameters:
#   - api_url: The URL of the API endpoint.
#   - token: The token to be used for authentication.
#   - silent: A logical value indicating whether to print project details or not. Default is FALSE.
#
# Returns:
#   - If silent is TRUE, it returns the response content as parsed data.
#   - If silent is FALSE, it prints the project details and returns NULL.
#
test_connection <- function(api_url, token, silent = FALSE) {
  data <- list(
    token = token,
    content = 'project',
    format = 'json',
    returnFormat = 'json'
  )
  
  r <- httr::POST(api_url, body = data, encode = "form")
  
  if (substr(r$status_code, 1, 1) == '2') {
    if (!silent) {
      cat("Connection successful!\n")
      cat("Project details:\n")
      print(httr::content(r))
    }
  } else {
    cat('Could not connect.\n')
    cat('HTTP Status: ', r$status_code, '\n')
  }
  
  if (silent) {
    return(httr::content(r, as = "parsed"))
  }
}
