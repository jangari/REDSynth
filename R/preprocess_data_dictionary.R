preprocess_data_dictionary <- function(data_dictionary) {
  # Preprocess the data dictionary and create a mapping of field names
  # to functions
  field_mapping <- list()
  
  for (field in data_dictionary) {
    field_name <- field$field_name
    field_type <- field$field_type
    field_validation <- field$text_validation_type_or_show_slider_number
    field_annotation <- field$field_annotation
    
    # Custom functions
    pattern <- ".*@REDSYNTH='(.*?)'"
    if (stringr::str_detect(field_annotation, pattern)){
      custom_function_name <- sub(pattern, "\\1", field_annotation)
      if (exists(custom_function_name, mode = "function")) {
        custom_function <- get(custom_function_name)
        field_mapping[[field_name]] <- custom_function
        message <- paste(custom_function_name, "found.")
        print(message)
        next
      } else {
        message <- paste("Custom function",custom_function_name,"not found. Defaulting to field type and validation.")
        print(message)
      }
    }
    
    # Add logic to map field names to functions based on field type, validation type, etc.
    if (field_type == "text") {
      # Determine based on the field validation type what kind of text field this is
      if (field_validation %in% c("date_dmy", "date_mdy", "date_ymd", "datetime_dmy", "datetime_mdy", "datetime_seconds_dmy", "datetime_seconds_mdy", "datetime_seconds_ymd", "datetime_ymd")) {
        field_mapping[[field_name]] <- generate_date_field_value
      } else if (field_validation %in% c("time", "time_hh_mm_ss", "time_mm_ss")) {
        field_mapping[[field_name]] <- generate_time_field_value
      } else if (field_validation == "number") {
        field_mapping[[field_name]] <- generate_number_field_value
      } else if (field_validation == "mobile_australia") {
        field_mapping[[field_name]] <- generate_mobile_aus_field_value
      } else if (field_validation == "phone_australia") {
        field_mapping[[field_name]] <- generate_phone_aus_field_value
      } else if (field_validation == "phone") {
        field_mapping[[field_name]] <- generate_phone_field_value
      } else if (field_validation == "postalcode_australia") {
        field_mapping[[field_name]] <- generate_postcode_aus_field_value
      } else if (field_validation == "zipcode") {
        field_mapping[[field_name]] <- generate_zipcode_field_value
      } else if (field_validation == "integer") {
        field_mapping[[field_name]] <- generate_integer_field_value
      } else if (field_validation == "email") {
        field_mapping[[field_name]] <- generate_email_field_value
      } else {
        field_mapping[[field_name]] <- generate_text_field_value
      }
    } else if (field_type %in% c("yesno", "truefalse")) {
      field_mapping[[field_name]] <- generate_boolean_field_value
    } else if (field_type %in% c("radio", "dropdown")) {
      field_mapping[[field_name]] <- generate_radio_dropdown_field_value
    } else if (field_type == "checkbox") {
      field_mapping[[field_name]] <- generate_checkbox_field_value
    } else if (field_type == "slider") {
      field_mapping[[field_name]] <- generate_slider_field_value
    } else if (field_type == "notes") {
      field_mapping[[field_name]] <- generate_notes_field_value
    }
    #Add more field type mappings as needed
  }
  
  return(field_mapping)
}
