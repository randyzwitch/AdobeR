#' Get user-defined segments for a company account
#'
#' @param id (character) Segment ID to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#' @param rsids (character) Only include segments tied to specified RSID list
#'   (comma-delimited)
#' @param segmentFilter (character) Only include segments in the specified list
#'   (comma-delimited list of IDs)
#' @param locale (character) Locale for encoding/localized spelling
#' @param name (character) Only include segments that contains the name
#' @param tagNames (character) Only include segments that contains one of the tags
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#' @param expansion (character) Comma-delimited list of additional metadata
#'   fields to include on response
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
GetSegments <- function(id=NULL,
                        as.data.frame=TRUE,
                        rsids=NULL,
                        segmentFilter=NULL,
                        locale=NULL,
                        name=NULL,
                        tagNames=NULL,
                        limit=100,
                        page=0,
                        expansion=NULL) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/segments"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  query <- list(rsids=rsids,
                segmentFilter=segmentFilter,
                locale=locale,
                name=name,
                tagNames=tagNames,
                limit=limit,
                page=page,
                expansion=expansion)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

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
