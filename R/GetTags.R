#' GetTags
#'
#' @param globalCompanyId
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetTags <- function(globalCompanyId, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  r <- adobe_get(endpoint, "/tags", AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  class(r) <- "Tags"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}

#' @export
#' @keywords internal
as.data.frame.Tags <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
CompanyAccess <- function(x) {
  UseMethod("Tags", x)
}
