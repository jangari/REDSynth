generate_phone_aus_field_value <- function(field){
    phone_number <- paste0('0',sample(c(2,3,4,7,8), 1),sample(10000000:99999999, 1))
    return(phone_number)
}
