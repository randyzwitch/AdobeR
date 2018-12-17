#' GetCalculatedMetrics
#'
#' @param globalCompanyId
#' @param id
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetCalculatedMetrics <- function(globalCompanyId, id=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s", globalCompanyId)
  resource <- "/calculatedmetrics"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(id)){
    class(r) <- "CalculatedMetrics"
  } else {
    class(r) <- "CalculatedMetric"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
