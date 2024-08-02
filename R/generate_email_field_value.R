generate_email_field_value <- function(field) {
    # Generate synthetic data for email fields
    email_domain <- 'example.com'
    email_address <- gsub(" ", "_", tolower(paste0(randomNames::randomNames(which.names = 'first'), ".", randomNames::randomNames(which.names = 'last'), "@", email_domain)))
    return(email_address)
}
