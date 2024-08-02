generate_mobile_aus_field_value <- function(field){
    # Generate synthetic data for AU mobile numbers
    mobile_number <- paste0('04',sample(10000000:99999999, 1))
    return(mobile_number)
}
