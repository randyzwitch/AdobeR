#' GetDateRanges
#'
#' @param globalCompanyId
#' @param dateRangeId
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetDateRanges <- function(globalCompanyId, dateRangeId=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/dateranges"

  if(!is.null(dateRangeId)){
    resource <- paste(resource, "/", dateRangeId, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

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
