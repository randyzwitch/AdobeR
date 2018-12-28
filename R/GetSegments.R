#' GetSegments
#'
#' @param id (character) Segment ID to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (Segments | Segment)
#' @export
#'
#' @examples
#' \dontrun{
#' seg <- GetSegments()
#' seg.nodf <- GetSegments(as.data.frame = FALSE)
#' segid <- GetSegments("5433e4e6e4b02df70be4ac63")
#' segid.nodf <- GetSegments("5433e4e6e4b02df70be4ac63", FALSE)
#'
#' }
GetSegments <- function(id=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/segments"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(id)){
    class(r) <- "Segments"
  } else {
    class(r) <- "Segment"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
