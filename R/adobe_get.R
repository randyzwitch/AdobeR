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

  return(httr::content(r))
}
