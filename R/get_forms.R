get_forms <- function(api_url, token) {
  # Define the data payload
  data <- list(
    'token' = token,
    'content' = 'instrument',
    'format' = 'json',
    'returnFormat' = 'json'
  )
  
  # Make the HTTP POST request
  r <- httr::POST(api_url, body = data)
  
  # Check the HTTP status code
  if (r$status_code == 200) {
    # Parse the JSON response
    json_data <- httr::content(r, as = "parsed")
    
    # Extract the instrument names
    forms <- lapply(json_data, function(row) row$instrument_name)
    
    # Return the list of instrument names
    return(forms)
  } else {
    stop(paste("Error: HTTP Status", r$status_code))
  }
}
