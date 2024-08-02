#' Generate a random value for a radio or dropdown field
#'
#' This function generates a synthetic data value for a radio or dropdown field.
#' It takes a field object as input and returns a randomly selected choice from
#' the field's select_choices_or_calculations.
#'
#' @param field The field object representing the radio or dropdown field.
#' @return A randomly selected choice from the field's select_choices_or_calculations.
#'
#' @examples
#' field <- list(select_choices_or_calculations = "Choice 1, Choice 2, Choice 3")
#' generate_radio_dropdown_field_value(field)
#'
#' @export
generate_radio_dropdown_field_value <- function(field) {
  # Generate synthetic data for radio and dropdown fields
  choices_string <- field$select_choices_or_calculations
  choices <- strsplit(choices_string, "\\|")[[1]]  # Split choices by '|'
  choices <- sapply(choices, function(choice) trimws(strsplit(choice, ",")[[1]][1]))  # Extract choice names and trim whitespace
  return(sample(choices, 1))  # Randomly select one choice
}
