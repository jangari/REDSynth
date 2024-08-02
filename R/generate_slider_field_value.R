#' Generate a synthetic data value for a slider field
#'
#' This function generates a synthetic data value for a slider field based on the specified range.
#'
#' @param field The field object representing the slider field.
#' @return A random integer within the specified range.
#'
#' @examples
#' field <- list(text_validation_min = '0', text_validation_max = '100')
#' generate_slider_field_value(field)
#'
#' @export
generate_slider_field_value <- function(field) {
  # Generate synthetic data for slider fields
  min_val <- as.numeric(ifelse(field$text_validation_min == '', 0, field$text_validation_min))
  max_val <- as.numeric(ifelse(field$text_validation_max == '', 100, field$text_validation_max))
  return(sample(min_val:max_val, 1))  # Generate a random integer within the specified range
}
