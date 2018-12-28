#' GetMetrics
#'
#' @param rsid (character) ID of desired report suite
#' @param metric (character) ID of the metric for which to retrieve info
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (Metrics | Metric)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' metrics <- GetMetrics("zwitchdev")
#' metrics.nodf <- GetMetrics("zwitchdev", as.data.frame = FALSE)
#' metricspv <- GetMetrics("zwitchdev", "pageviews")
#' metricspv.nodf <- GetMetrics("zwitchdev", "pageviews", as.data.frame = FALSE)
#'
#' }
GetMetrics <- function(rsid, metric=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/metrics",
                      globalCompanyId)

  resource <- paste("?rsid=", rsid, sep="")

  if(!is.null(metric)){
    endpoint <- paste(endpoint, "/", metric, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(metric)){
    class(r) <- "Metrics"
  } else {
    class(r) <- "Metric"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
