get_repeating_events_forms <- function(api_url, token){
  formData <- list('token'=token,
                   'content'='repeatingFormsEvents',
                   'format'='json',
                   'returnFormat'='json'
  )
  response <- httr::POST(api_url, body = formData, encode = "form")
  result <- httr::content(response)
  print(result)
}
