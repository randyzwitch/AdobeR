#' GetCalculatedMetrics
#'
#' @param id (character) The calculated metric ID to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (CalculatedMetric | CalculatedMetrics)
#' @export
#'
#' @examples
#' \dontrun{
#' cm <- GetCalculatedMetrics()
#' cmid <- GetCalculatedMetrics("cm300005752_557fc500e4b013edc2a85531")
#'
#' #returns S3 CalculatedMetrics
#' cm.nodf <- GetCalculatedMetrics(as.data.frame=FALSE)
#' #returns S3 CalculatedMetric
#' cmid.nodf <- GetCalculatedMetrics("cm300005752_557fc500e4b013edc2a85531", as.data.frame = FALSE)
#'
#' }
GetCalculatedMetrics <- function(id=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s", globalCompanyId)
  resource <- "/calculatedmetrics"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

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
