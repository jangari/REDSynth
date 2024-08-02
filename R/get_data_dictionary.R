# Retrieve metadata
get_data_dictionary <- function(api_url, token) {
  project_info <- test_connection(api_url, token, silent = TRUE)
  project_title <- project_info$project_title
  project_pid <- project_info$project_id
  cat(sprintf("Retrieving data dictionary from project %s (PID %s).\n", project_title, project_pid))
  
  data <- list(
    token = token,
    content = 'metadata',
    format = 'json',
    returnFormat = 'json'
  )
  
  response <- httr::POST(api_url, body = data, encode = "form")

  if (substr(response$status_code, 1, 1) == '2') {
    json_data <- httr::content(response, as = "parsed")
    # Define a list to store parsed data
    parsed_data <- list()
    
    # Iterate over each field in the JSON data
    for (field in json_data) {
      parsed_field <- list(
        field_name = field$field_name,
        form_name = field$form_name,
        field_label = field$field_label,
        field_type = field$field_type,
        select_choices_or_calculations = field$select_choices_or_calculations,
        text_validation_type_or_show_slider_number = field$text_validation_type_or_show_slider_number,
        text_validation_min = field$text_validation_min,
        text_validation_max = field$text_validation_max,
        field_annotation = field$field_annotation
      )
      
      # Append the parsed field to the list
      parsed_data <- c(parsed_data, list(parsed_field))
    }
    
    return(parsed_data)
  } else {
    stop(sprintf("Error: HTTP Status %s", response$status_code))
  }
}
