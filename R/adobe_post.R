#' @export
#' @keywords internal
#' @noRd
adobe_post <- function(resource, body, query=NULL) {

  headers <-
    c(
      "x-api-key" = AdobeRInternals$auth$app$key ,
      "x-proxy-global-company-id" = AdobeRInternals$globalCompanyId,
      "Authorization" = sprintf("Bearer %s", AdobeRInternals$auth$credentials$access_token),
      "Accept" = "application/json",
      "Content-Type" = "application/json"
    )


  fullURL <- paste("https://analytics.adobe.io/api/",
                   AdobeRInternals$globalCompanyId,
                   resource, sep = "")

  r <- httr::POST(fullURL,
                  httr::add_headers(headers),
                  body = request,
                  query = query,
                  encode = "json")

  #representation as it came from API
  r_text <- httr::content(r, as = "text", encoding = "UTF-8")

  #parse JSON into data frame if possible
  parsed <- jsonlite::fromJSON(r_text, simplifyDataFrame = TRUE, flatten = TRUE)

  if(!httr::http_error(r)){

    #return original content, as well as the parsed response from jsonlite
    #mark class as AdobeRSuccess, check will allow for error checking elsewhere
    returnval <- list(json = r_text, response = parsed)
    class(returnval) <- "AdobeRSuccess"
    return(returnval)

  } else {

    #the stop here prevents needing to check the status in the caller function
    #either adobe_get returns the expected response or stop throws the error
    if(!is.null(parsed$errorCode)){
      stop(sprintf("Error %s: %s", parsed$errorCode, parsed$errorDescription))
    } else if(!is.null(parsed$error_code)){
      stop(sprintf("Error %s: %s", parsed$error_code, parsed$message))
    }

  }

}
