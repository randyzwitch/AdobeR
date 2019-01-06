#' Get user-defined tags for a company account
#'
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 'Tags'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' gtag <- GetTags()
#' gtag.nodf <- GetTags(as.data.frame = FALSE) #returns S3 Tags
#' }
GetTags <- function(as.data.frame=TRUE) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/tags"

  r <- adobe_get(endpoint, resource, globalCompanyId)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "Tags")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
