#' @description Returns companies user can access, based on Token 2.0 credentials
#'
#' @title Companies user id can access
#'
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 object of class "CompanyAccess"
#' @export
#'
#' @seealso \code{\link{AdobeOAuth}}
#' @examples
#' \dontrun{
#'
#' df <- GetUserCompanyAccess()
#' discovery <- GetUserCompanyAccess(as.data.frame=FALSE)
#' }
GetUserCompanyAccess <- function(as.data.frame=TRUE) {
  r <-
    adobe_get("https://analytics.adobe.io",
              "/discovery/me")

  #Set S3 method for easier parsing later
  class(r) <- "CompanyAccess"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
