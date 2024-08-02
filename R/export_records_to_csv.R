#' Export Records to CSV
#'
#' This function takes a JSON string of records and exports them to a CSV file.
#'
#' @param json_records A JSON string containing the records to export.
#' @param filename The name of the CSV file to create.
#'
#' @return The number of records exported, or 0 if there are no records to export.
#'
#' @examples
#' # Example usage:
#' json_records <- '[{"id": 1, "name": "John"}, {"id": 2, "name": "Jane"}]'
#' filename <- "records.csv"
#' export_records_to_csv(json_records, filename)
#'
#' @import jsonlite
#' @export
export_records_to_csv <- function(json_records, filename) {
  # Load JSON data
  records <- jsonlite::fromJSON(json_records)
  
  # Check if records is empty
  if (length(records) == 0) {
    print("No records to export.")
    return(0)
  }
  
  # Extract field names from the first record
  fieldnames <- names(records[[1]])
  
  # Write records to CSV file
  write.csv(records, file = filename, row.names = FALSE, na = "")
  
  return(length(records))
}
