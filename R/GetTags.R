#' GetTags
#'
#' @param id (character) ID for tag to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (Tags | Tag)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' gtag <- GetTags()
#' gtag.nodf <- GetTags(as.data.frame = FALSE) #returns S3 Tags
#' gtagid <- GetTags("28638")
#' gtagid.nodf <- GetTags("28638", as.data.frame = FALSE)
#' }
GetTags <- function(id=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/tags"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(id)){
    class(r) <- "Tags"
  } else {
    class(r) <- "Tag"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
