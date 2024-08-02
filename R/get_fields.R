get_fields <- function(api_url, token) {
  # Define the data payload
  data <- list(
    'token' = token,
    'content' = 'exportFieldNames',
    'format' = 'json',
    'returnFormat' = 'json'
  )
  
  # Make the HTTP POST request
  r <- httr::POST(api_url, body = data)
  
  # Check the HTTP status code
  if (r$status_code == 200) {
    # Parse the JSON response
    json_data <- httr::content(r, as = "parsed")
    
    # Extract the original field names
    fields <- lapply(json_data, function(row) row$original_field_name)
    
    # Remove _complete fields and duplicate field names for checkbox fields
    fields <- unique(grep("_complete$", unlist(fields), invert = TRUE, value = TRUE))
    
    # Return the list of field names
    return(fields)
  } else {
    stop(paste("Error: HTTP Status", r$status_code))
  }
}
