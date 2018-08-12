#' @export
#' @keywords internal
adobe_get <- function(baseurl, endpoint, auth) {
  #TODO: Input validation

  #Set headers
  headers <-
    c(
      'Authorization' = sprintf("Bearer %s", auth$credentials$access_token),
      'x-api-key' = auth$app$key,
      'Accept' = 'application/json'
    )

  #Build URL
  fullURL <- paste(baseurl, endpoint, sep = "")

  #Make API call
  r <- httr::GET(fullURL, httr::add_headers(headers))

  #TODO: check response code, proceed differently based on code

  #representation as it came from API
  r_text <- httr::content(r, as = "text", encoding = "UTF-8")

  #parse JSON into data frame if possible
  parsed <- jsonlite::fromJSON(r_text, simplifyDataFrame = TRUE, flatten = TRUE)

  #return original content, as well as the parsed response from jsonlite
  return(list(json = r_text, response = parsed))
}
