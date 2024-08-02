is_repeating_event_or_form <- function(
    is_longitudinal, 
    event, 
    form, 
    repeating_event_forms
) {
  if (!is_longitudinal) {
    for (mapping in repeating_event_forms) {
      if (mapping$form_name == form) {
        return(1)
      }
    }
    return(0)
  } else {
    for (mapping in repeating_event_forms) {
      if (mapping$event_name == event) {
        if (mapping$form_name == "") {
          return(2)
        } else if (mapping$form_name == form) {
          return(1)
        }
      }
    }
    return(0)
  }
}