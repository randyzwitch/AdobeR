#' @export
#' @keywords internal
adobe_get <- function(baseurl, endpoint, auth, globalCompanyId = NULL) {
  #TODO: Input validation

  #Set headers
  headers <-
    c(
      'Authorization' = sprintf("Bearer %s", auth$credentials$access_token),
      'x-api-key' = auth$app$key,
      'Accept' = 'application/json'
    )

  if(!is.null(globalCompanyId)){
    headers <- c(headers, 'x-proxy-global-company-id' = globalCompanyId)
  }

  #Build URL
  fullURL <- paste(baseurl, endpoint, sep = "")

  #Make API call
  r <- httr::GET(fullURL, httr::add_headers(headers))

  #TODO: check response code, proceed differently based on code

  #representation as it came from API
  r_text <- httr::content(r, as = "text", encoding = "UTF-8")

  #parse JSON into data frame if possible
  parsed <- jsonlite::fromJSON(r_text, simplifyDataFrame = TRUE, flatten = TRUE)

  #Check to see if Adobe returns an error message
  if(!is.null(parsed$errorCode)){
    stop(sprintf("Error %s: %s", parsed$errorCode, parsed$errorDescription))
  } else if(!is.null(parsed$error_code)){
    stop(sprintf("Error %s: %s", parsed$error_code, parsed$message))
  }

  #return original content, as well as the parsed response from jsonlite
  return(list(json = r_text, response = parsed))
}
