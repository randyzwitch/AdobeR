#' GetDateRanges
#'
#' @param dateRangeId (character) The DateRange id for which to retrieve information
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (DateRanges | DateRange)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' gdr <- GetDateRanges()
#' gdr.nodf <- GetDateRanges(as.data.frame = FALSE)
#' gdrid <- GetDateRanges("5c16f34fc66baa47fdd93804")
#' gdrid.nodf <- GetDateRanges("5c16f34fc66baa47fdd93804", as.data.frame = FALSE)
#'
#' }
GetDateRanges <- function(dateRangeId=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/dateranges"

  if(!is.null(dateRangeId)){
    resource <- paste(resource, "/", dateRangeId, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(dateRangeId)){
    class(r) <- "DateRanges"
  } else {
    class(r) <- "DateRange"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
