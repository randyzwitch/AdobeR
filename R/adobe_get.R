#' adobe_get
#'
#' @param datacenter datacenter
#' @param endpoint endpoint
#' @param auth auth
#'
#' @return character
#' @export
#'
#' @examples
#'
adobe_get <- function(datacenter, endpoint, auth) {
  fullURL <- paste(datacenter, endpoint, sep = "")

  r <- httr::GET(fullURL, httr::add_headers(Authorization = auth))

  #representation as it came from API
  r_text <- httr::content(r, as = "text", encoding = "UTF-8")

  #parse JSON into data frame if possible
  parsed <- jsonlite::fromJSON(r_text, simplifyDataFrame = TRUE, flatten = TRUE)

  return(c(response = r_text, parsed))
}
