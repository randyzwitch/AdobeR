#' Companies a user id can access
#'
#' @description Returns companies user can access, based on Token 2.0 credentials
#'
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (CompanyAccess)
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

  assertthat::assert_that(is.logical(as.data.frame),
              msg="as.data.frame required to be class 'logical'")

  r <- adobe_get("https://analytics.adobe.io", "/discovery/me")

  #Set S3 method for easier parsing later
  class(r) <- "CompanyAccess"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
