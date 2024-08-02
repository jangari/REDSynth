generate_synthetic_data <- function(data_dictionary, event_forms, events, forms, fields, num_records, start_num) {

  # Preprocess the data dictionary
  field_mapping <- preprocess_data_dictionary(data_dictionary)
  
  # Get the Record ID field name
  record_id_field <- data_dictionary[[1]]$field_name
  
  # Initialize lists to store information
  records <- list()
  skipped_fields <- list()
  handled_fields <- list()
  ineligible_fields <- list()
  ineligible_field_types <- c('descriptive', 'sql', 'calc', 'file')  # Turns out in the metadata, signature fields are just file upload fields with 'signature' validation.
  calctext_fields <- list() 
  calcdate_fields <- list() 
  eventForms <- list()
  
  # Check if events is empty and set a placeholder value if it is. Required to allow both longitudinal and classic to work.
  if (length(events) == 0) {
    events <- "placeholder"
  }
  
  # Loop through each record
  for (idx in 1:num_records) {
    # Loop through events
    for (event in events) {
      # Could think about another loop here to generate another row per instance of a repeating instrument, for (inst in sample(min:max,1)), and if not repeating, the min and max both = 1. Where do min and max come from otherwise? Maybe set in synthesise random_instances_min and random_instances_max, both default 1, error if max < min,
      synthetic_record <- list()  # Instantiate a new row of the import per record-event
      synthetic_record[[record_id_field]] <- idx + start_num - 1  # Start at record start_num or 1.
      
      if (event != 'placeholder') {  # This is TRUE if this is a longitudinal project, thus allowing the event-form layer to take effect.
        eventForms <- intersect(event_forms[[event]], forms)  # Limit forms to those in the intersection of those requested and those designated on the current event

        if (length(eventForms) == 0) {  # No forms in this event, don't create data that just confuses the logging
          next
        }
    
        synthetic_record[['redcap_event_name']] <- event  # All of the above wrangling to get this one critical value in the record data.
      }
      
      # Loop through each field in the data dictionary
      for (field_info in data_dictionary) {
        form_name <- field_info$form_name
        field_name <- field_info$field_name
        field_type <- field_info$field_type
        field_annotation <- field_info$field_annotation
        
        # if (has_repeating_instruments_or_events){
        # # Determine if this event and/of form is repeating
        #   repeating_event_or_form <- is_repeating_event_or_form(is_longitudinal, event, form_name, repeating_events_forms)
        # 
        #   if (repeating_event_or_form==1) {  # Repeating instrument, so add the necessary field values
        #     # Append the synthesized record to the list and start another one
        #     records <- c(records, list(synthetic_record))
        #     synthetic_record <- list()
        #     synthetic_record[['redcap_repeat_instrument']] <- form_name
        #     synthetic_record[['redcap_repeat_instance']] <- 'new'
        #   } else if (repeating_event_or_form==2){  # Repeating event, don't include form name.
        #     # Append the synthesized record to the list and start another one
        #     records <- c(records, list(synthetic_record))
        #     synthetic_record <- list()
        #     synthetic_record[['redcap_repeat_instance']] <- 'new'
        #   }
        # }
        
        if (grepl("@CALCTEXT", field_annotation)) {
          calctext_fields <- c(ineligible_fields, field_name)  # Cannot import @CALCTEXT
          next
        } 
        
        if (grepl("@CALCDATE", field_annotation)) {
          calcdate_fields <- c(ineligible_fields, field_name)  # Cannot import @CALCDATE
          next
        } 
        
        if (field_name == record_id_field) {
          next  # Skip the Record ID field
        }
        
        if (field_type %in% ineligible_field_types) {
          ineligible_fields <- c(ineligible_fields, field_name)
          next  # Skip ineligible fields
        }
        
        if (!(length(fields) == 0 || field_name %in% fields)) {
          skipped_fields <- c(skipped_fields, field_name)
          next  # Skip fields not included in requested subset
        }
        
        if (!(form_name %in% forms)) {
          skipped_fields <- c(skipped_fields, field_name)
          next  # Skip fields not included in requested forms
        }
        
        if (!(length(eventForms) == 0 || form_name %in% eventForms)) {
          skipped_fields <- c(skipped_fields, field_name)
          next  # Skip fields not included in current event's forms
        }
        
        # Try to generate synthetic data based on the field metadata
        tryCatch({
          generate_function <- field_mapping[[field_name]]
          synthetic_value <- generate_function(field_info)
          synthetic_record[[field_name]] <- synthetic_value
          handled_fields <- c(handled_fields, field_name)
        }, error = function(e) {
          # Handle errors for fields that cannot be synthesized
          print(paste("Warning: Field ", field_name, " (type ", field_type, ") cannot be handled. Skipping.", sep = ""))
        })
      }
    
      # Append the synthesized record to the list
      records <- c(records, list(synthetic_record))
    }
  }
  
  # Generate report for handled fields
  report <- "Data Synthesis Report:\n"
  for (field_info in data_dictionary) {
    field_name <- field_info$field_name
    if (field_name == record_id_field) {
      report <- paste(report, paste("- ", field_name, ": Record ID field\n", sep = ""))
    } else if (field_name %in% handled_fields) {
      report <- paste(report, paste("- ", field_name, ": Synthesized as field type ", field_info$field_type, "\n", sep = ""))
    } else if (field_name %in% ineligible_fields) {
      report <- paste(report, paste("- ", field_name, ": Field type ", field_info$field_type, " ineligible\n", sep = ""))
    } else if (field_name %in% skipped_fields) {
      report <- paste(report, paste("- ", field_name, ": Field type ", field_info$field_type, " skipped\n", sep = ""))
    } else if (field_name %in% calctext_fields) {
      report <- paste(report, paste("- ", field_name, ": Field with @CALCTEXT annotation skipped\n", sep = ""))
    } else if (field_name %in% calcdate_fields) {
      report <- paste(report, paste("- ", field_name, ": Field with @CALCDATE annotation skipped\n", sep = ""))
    } else {
      report <- paste(report, paste("- ", field_name, ": Field type ", field_info$field_type, " unknown\n", sep = ""))
    }
  }
  
  # Print and return the report and records
  cat(report)
  return(records)
}
