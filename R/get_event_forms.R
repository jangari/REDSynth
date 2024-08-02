get_event_forms <- function(api_url, token){
  formData <- list('token'=token,
                   'content'='formEventMapping',
                   'format'='json',
                   'returnFormat'='json'
  )
  response <- httr::POST(api_url, body = formData, encode = "form")
  result <- httr::content(response)
  
  # Initialize an empty list to store the mappings
  event_form_list <- list()
  
  # Iterate over the event-form mappings
  for (mapping in result) {
    # Extract the unique_event_name and form
    unique_event_name <- mapping$unique_event_name
    form <- mapping$form
    
    # Check if the unique_event_name already exists in the list
    if (unique_event_name %in% names(event_form_list)) {
      # Append the form to the existing list
      event_form_list[[unique_event_name]] <- c(event_form_list[[unique_event_name]], form)
    } else {
      # Create a new entry for the unique_event_name
      event_form_list[[unique_event_name]] <- list(form)
    }
  }
  return(as.list(event_form_list))  
}
