generate_date_field_value <- function(field) {
  # Generate synthetic data for date fields
  field_label <- tolower(field$field_label)
  validation_type <- field$text_validation_type_or_show_slider_number
  min_val <- field$text_validation_min
  max_val <- field$text_validation_max
  
  # If min and max are unset, 0 and 100 are assumed.
  min_val <- ifelse(min_val == '', 0, as.numeric(min_val))
  max_val <- ifelse(max_val == '', 100, as.numeric(max_val))
  
  # Swap min and max if they are inconsistent. This may produce output that is out of range, but it will import.
  if (min_val > max_val) {
    temp_max <- min_val
    min_val <- max_val
    max_val <- temp_max
  }
  
  # date validated fields
    date_format_mapping <- list(
      'date_dmy' = '%Y-%m-%d',
      'date_mdy' = '%Y-%m-%d',
      'date_ymd' = '%Y-%m-%d',
      'datetime_dmy' = '%Y-%m-%d %H:%M',
      'datetime_mdy' = '%Y-%m-%d %H:%M',
      'datetime_seconds_dmy' = '%Y-%m-%d %H:%M:%S',
      'datetime_seconds_mdy' = '%Y-%m-%d %H:%M:%S',
      'datetime_seconds_ymd' = '%Y-%m-%d %H:%M:%S',
      'datetime_ymd' = '%Y-%m-%d %H:%M'
    )
    strftime_param <- date_format_mapping[[validation_type]]
    if (grepl('dob|birth', field_label) || grepl('dob|birth', field_name)) {
      return(as.character(Sys.Date() - sample(1:365*100, 1)))
    } else {
      return(as.character(Sys.Date() - sample(1:365*10, 1)))
    }
}
