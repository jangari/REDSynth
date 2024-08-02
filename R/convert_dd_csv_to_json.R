convert_dd_csv_to_json <- function(csv_filename) {
  json_data <- list()
  
  # Read CSV file
  data <- read.csv(csv_filename, stringsAsFactors = FALSE)
  
  # Convert each row to a dictionary and append to json_data list
  for (i in 1:nrow(data)) {
    field <- list(
      'field_name' = data[i, 'Variable / Field Name'],
      'field_label' = data[i, 'Field Label'],
      'field_type' = data[i, 'Field Type'],
      'select_choices_or_calculations' = data[i, 'Choices, Calculations, OR Slider Labels'],
      'text_validation_type_or_show_slider_number' = data[i, 'Text Validation Type OR Show Slider Number'],
      'text_validation_min' = data[i, 'Text Validation Min'],
      'text_validation_max' = data[i, 'Text Validation Max']
    )
    json_data <- c(json_data, list(field))
  }
  
  return(json_data)
}
