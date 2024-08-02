synthesize <- function(api_url,
                       token,
                       dd_method='api',
                       dd_filename=NULL,
                       event_forms,
                       events=NULL,
                       forms=NULL,
                       fields=NULL,
                       num_records=10,
                       output='print',
                       csv_filename=NULL,
                       dry_run=FALSE,
                       forceAutoNumber=TRUE,
                       start_num=1)
{
  # Defensive checks
  print(dd_method)
  stopifnot("dd_method must be 'api' or 'file'."= dd_method %in% c('api','file'))
  stopifnot("output must be 'print', 'csv', or 'api'."= output %in% c('print', 'csv', 'api'))
  stopifnot("num_records must be greater than zero."= num_records > 0)
  stopifnot("forceAutoNumber must be of type logical."= is.logical(forceAutoNumber))
  stopifnot("start_num must be greater than zero."= start_num > 0)
  
  # Collect some project info
  project_info <- test_connection(api_url, token, silent=TRUE)
  project_title <- project_info$project_title
  project_pid <- project_info$project_id
  has_repeating_instruments_or_events <- project_info$has_repeating_instruments_or_events
  is_longitudinal <- project_info$is_longitudinal
  #repeating_event_forms <- list()
  
  # Get repeating events and forms
  # Abandoning repeating forms and events for now.
  #if (has_repeating_instruments_or_events) {
  #  repeating_event_forms <- get_repeating_events_forms(api_url, token)
  #}
  
  # Collect data dictionary
  if (dd_method == 'api') {
    data_dictionary <- get_data_dictionary(api_url, token)
  } else if (dd_method == 'file') {
    data_dictionary <- convert_dd_csv_to_json(dd_filename)
  }
  #print(data_dictionary)
  
  # If longitudinal, get events, else events_forms = '0'
  # Check if the project is longitudinal
  if (project_info$is_longitudinal == 1) {
    event_forms <- get_event_forms(api_url, token)
  } else {
    event_forms <- 0  # Or any other value that indicates no longitudinal events
  }  
  
  if (is.null(events) || length(events) == 0) {
    events <- names(event_forms)
  }
  
  # Populate fields and forms lists if empty
  if (is.null(fields) || length(fields) == 0) {
    tryCatch({
      fields <- get_fields(api_url, token)
    }, error = function(e) {
      print("Could not get field names. Defaulting to All.")
    })
  }
  if (is.null(forms) || length(forms) == 0) {
    tryCatch({
      forms <- get_forms(api_url, token)
    }, error = function(e) {
      print("Could not get form names. Defaulting to All.")
    })
  }
  
  synthetic_data <- generate_synthetic_data(data_dictionary, event_forms, events, forms, fields, num_records, start_num)
  flattened_data <- flatten_nested_dicts(synthetic_data)
  json_data <- jsonlite::toJSON(flattened_data, auto_unbox = TRUE)
  
  if (dry_run) {
    print("Dry run. No data generated.")
  } else {
    if ('api' %in% output) {

      if (project_info$in_production == 1) {
        print("Warning: This project is in production. Cannot import synthetic data.")
        return()
      }
      #if (is_longitudinal) {
      #  print("Warning: This project has longitudinal data collection enabled. This is not yet supported and can only import data into the first event.")
      #  #return()
      #}
      if (has_repeating_instruments_or_events) {
        print("Warning: This project has repeating instruments or events. This is not yet supported, and import will fail if it contains any fields that are on repeating instruments or events.")
        #return()
      }
      if (project_info$randomization_enabled == 1) {
        print("Warning: This project has randomization enabled. You cannot import values into the randomization field. Currently there is no way to determine the randomization field via the API. The import will be attempted, but if an error occurs, you may want to check the randomization field and exclude it by including all other fields in the `fields` parameter.")
      }
      tryCatch({
        response <- upload_record_to_redcap(api_url, token, json_data, forceAutoNumber)
        print(paste("Success! Synthesised ", response$count, " records and imported into project ", project_title, " (PID ", project_pid, ").", sep = ""))
      }, error = function(e) {
        print(paste("Error:", e$message))
      })
    } 
    if ('csv' %in% output) {
      csv_count <- export_records_to_csv(json_data, csv_filename)
      print(paste("Success! Created", csv_count, "records and saved as", csv_filename, "."))
    } 
    if ('print' %in% output) {
      print(paste("Success! Created", length(flattened_data), "records."))
      print(json_data)
    }
  }
}
