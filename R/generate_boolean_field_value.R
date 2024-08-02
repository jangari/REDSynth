#' Generate a random boolean field value.
#'
#' This function generates a random boolean field value (0 or 1).
#'
#' @param field The field for which the boolean value is generated.
#' @return A random boolean value (0 or 1).
#' @examples
#' generate_boolean_field_value("example_field")
generate_boolean_field_value <- function(field) {
  return(sample(c(0,1),1))
}
