generate_zipcode_field_value <- function(field){
    # Generate synthetic data for UZ zip codes
    postcode <- paste0(sample(10000:99999, 1))
    return(postcode)
}
