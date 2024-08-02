generate_time_field_value <- function(field){
    validation_type <- field$text_validation_type_or_show_slider_number
    random_hours <- sample(0:23, 1)
    # Add zero padding to hours
    if (nchar(random_hours) == 1) {
        random_hours <- paste0("0", random_hours)
    }
    random_minutes <- sample(0:59, 1)
    # Add zero padding to minutes
    if (nchar(random_minutes) == 1) {
        random_minutes <- paste0("0", random_minutes)
    }
    random_seconds <- sample(0:59, 1)
    # Add zero padding to seconds
    if (nchar(random_seconds) == 1) {
        random_seconds <- paste0("0", random_seconds)
    }
    if (validation_type == "time") {
        return(paste0(random_hours, ':', random_minutes))
    } else if (validation_type == "time_hh_mm_ss") {
        return(paste0(random_hours, ':', random_minutes, ':', random_seconds))
    } else if (validation_type == "time_mm_ss") {
        return(paste0(random_minutes, ':', random_seconds))
    }
}
