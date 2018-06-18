#' GetUserById
#'
#' @param id User id to retrieve
#'
#' @return AnalyticsUser
#' @export
#'
#' @examples
#' \dontrun{
#' gubi <- GetUserById()
#' }
GetUserById <- function(id="me"){
  endpoint <- paste("users/", id, sep = "")
  resp <- adobe_get(datacenter, endpoint, auth = auth)
  return(AnalyticsUser$new(resp))
}
