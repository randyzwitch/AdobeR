#' @keywords internal
#' @noRd
adobe_get <- function(endpoint, resource, globalCompanyId = NULL, query = NULL) {

  headers <-
    c(
      'Authorization' = sprintf("Bearer %s", AdobeRInternals$auth$credentials$access_token),
      'x-api-key' = AdobeRInternals$auth$app$key,
      'Accept' = 'application/json'
    )

  #All but discovery endpoint requires global-company-id header
  if(!is.null(globalCompanyId)){
    headers <- c(headers, 'x-proxy-global-company-id' = globalCompanyId)
  }

  fullURL <- paste(endpoint, resource, sep = "")

  r <- httr::GET(fullURL, httr::add_headers(headers), query = query)

  if(!httr::http_error(r)){

    #representation as it came from API
    r_text <- httr::content(r, as = "text", encoding = "UTF-8")

    #parse JSON into data frame if possible
    parsed <- jsonlite::fromJSON(r_text, simplifyDataFrame = TRUE, flatten = TRUE)

    #return original content, as well as the parsed response from jsonlite
    #mark class as AdobeRSuccess, check will allow for error checking elsewhere
    returnval <- list(json = r_text, response = parsed)
    class(returnval) <- "AdobeRSuccess"
    return(returnval)

  } else {

    #TODO: check if token expired, refresh, then run again
    print(httr::http_status(r))

    #Check to see if Adobe returns an error message
    if(!is.null(parsed$errorCode)){
      stop(sprintf("Error %s: %s", parsed$errorCode, parsed$errorDescription))
    } else if(!is.null(parsed$error_code)){
      stop(sprintf("Error %s: %s", parsed$error_code, parsed$message))
    }

  }

}
