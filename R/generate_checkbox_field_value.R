#' Generate synthetic data for checkbox fields.
#'
#' This function generates synthetic data for checkbox fields. It takes a field object as input and returns a list of checkbox values.
#' The function randomly selects which options are checked based on the choices provided in the field object.
#'
#' @param field The field object representing the checkbox field.
#' @return A list of checkbox values, where each value represents whether the corresponding option is checked (1) or not checked (0).
#' @examples
#' field <- list(select_choices_or_calculations = "Option 1 | Option 2 | Option 3")
#' generate_checkbox_field_value(field)
#' @export
generate_checkbox_field_value <- function(field) {
  choices_string <- field$select_choices_or_calculations
  choices <- strsplit(strsplit(choices_string, " \\| ")[[1]], ", ")
  checkbox_values <- list()
  for (choice in choices) {
    code <- choice[1]
    checkbox_values[code] <- sample(c(0, 1), 1)
  }
  return(checkbox_values)
}
