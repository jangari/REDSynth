generate_text_field_value <- function(field) {
  # Generate synthetic data for unvalidated text fields
  field_name <- field$field_name
  field_label <- tolower(field$field_label)
    # Handle other text types. Address, job, title, etc.
    if ('name' %in% unlist(strsplit(field_label, " ")) && any(c('first', 'fname', 'personal', 'given', 'christian') %in% unlist(strsplit(field_label, " ")))) {
        # Return a random first name
        return(randomNames::randomNames(which.names = 'first'))
    } else if ('name' %in% unlist(strsplit(field_label, " ")) && any(c('last', 'family', 'lname', 'surname') %in% unlist(strsplit(field_label, " ")))) {
        # Return a random last name
        return(randomNames::randomNames(which.names = 'last'))
    } else if (any(c('title', 'role') %in% unlist(strsplit(field_label, " ")))) {
        # Return one of a list of common English personal titles, like Mr, Mrs, Dr, etc
        return(sample(c('Mr', 'Mrs', 'Dr', 'Prof', 'Rev', 'Ms', 'Mx'), 1))
    } else if (any(c('job', 'position') %in% unlist(strsplit(field_label, " ")))) {
        # Return one of a list of common English professions, like teacher, physician, etc
        return(sample(c('teacher', 'doctor', 'engineer', 'nurse', 'janitor'), 1))
    } else if (any(c('address', 'street') %in% unlist(strsplit(field_label, " ")))) {
        # Return a random integer between 1 and 999 followed by a random surname from randomNames, followed by one of a list of common street types, like St, Rd, Ave, Cct, Ct, etc
        return(paste0(sample(c(1:999), 1), ' ', randomNames::randomNames(which.names = 'last'), ' ', sample(c('St', 'Rd', 'Ave', 'Cct', 'Ct', 'Ln', 'Pl', 'Cir', 'Blvd'), 1)))
    } else {
        # Otherwise insert 7 words from Lorem Ipsum
        #return(lorem::ipsum_words(n = 7))
        return(as.character(lorem::ipsum(sentences = 1)))
    }
}
