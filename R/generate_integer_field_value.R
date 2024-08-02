generate_integer_field_value <- function(field) {
    # Generate synthetic data for integer fields
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
    
    return(sample(min_val:max_val, 1))
}
