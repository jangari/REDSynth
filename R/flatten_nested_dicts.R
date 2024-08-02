#' Flatten Nested Dictionaries
#'
#' This function takes a list of nested dictionaries generated to populate checkbox
#' fields and flattens them into a list of dictionaries where each key-value pair is
#' at the top level, so as to be compliant with REDCap's import structure. If a value
#' is a nested dictionary, it is recursively #' flattened and the keys are concatenated
#' with the parent keys using "___" as a separator.
#'
#' @param data_list A list of nested dictionaries.
#'
#' @return A list of flattened dictionaries.
#'
#' @examples
#' data_list <- list(
#'   list(a = 1, b = list(c = 2, d = 3)),
#'   list(e = 4, f = list(g = 5, h = 6))
#' )
#' flattened_data_list <- flatten_nested_dicts(data_list)
#' print(flattened_data_list)
#'
#' @export
flatten_nested_dicts <- function(data_list) {
  flattened_data_list <- list()
  
  # Loop through each data dictionary in the list
  for (data in data_list) {
    flattened_data <- list()
    
    # Loop through each key-value pair in the dictionary
    for (key in names(data)) {
      value <- data[[key]]
      
      # If the value is a nested dictionary, flatten it
      if (is.list(value)) {
        for (nested_key in names(value)) {
          flattened_key <- paste(key, nested_key, sep = "___")
          flattened_data[[flattened_key]] <- value[[nested_key]]
        }
      } else {
        flattened_data[[key]] <- value
      }
    }
    
    # Append the flattened dictionary to the list
    flattened_data_list <- c(flattened_data_list, list(flattened_data))
  }
  
  return(flattened_data_list)
}
