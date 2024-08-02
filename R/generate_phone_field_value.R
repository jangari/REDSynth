generate_phone_field_value <- function(field){
    phone_number <- paste0(sample(2000000000:9999999999, 1))
    return(phone_number)
}
