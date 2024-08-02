generate_postcode_aus_field_value <- function(field){
    # Generate synthetic data for postcode fields
    postcode <- paste0(sample(1000:9999, 1))
    return(postcode)
}
