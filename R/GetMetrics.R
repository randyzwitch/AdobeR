#' GetMetrics
#'
#' @param globalCompanyId
#' @param rsid
#' @param metric
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetMetrics <- function(globalCompanyId, rsid, metric=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/metrics",
                      globalCompanyId)
  resource <- paste("?rsid=", rsid, sep="")

  if(!is.null(metric)){
    endpoint <- paste(endpoint, "/", metric, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

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
