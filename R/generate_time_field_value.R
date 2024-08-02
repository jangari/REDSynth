generate_time_field_value <- function(field){
    validation_type <- field$text_validation_type_or_show_slider_number
    random_hours <- sample(0:23, 1)
    random_minutes <- sample(0:59, 1)
    random_seconds <- sample(0:59, 1)
    if (validation_type == "time") {
        return(paste0(random_hours, ':', random_minutes))
    } else if (validation_type == "time_hh_mm_ss") {
        return(paste0(random_hours, ':', random_minutes, ':', random_seconds))
    } else if (validation_type == "time_mm_ss") {
        return(paste0(random_minutes, ':', random_seconds))
    }
}
