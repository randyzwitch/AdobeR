#' GetReportSuites
#'
#' @param rsid
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetReportSuites <- function(rsid=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/collections/suites"

  if(!is.null(rsid)){
    resource <- paste(resource, "/", rsid, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(rsid)){
    class(r) <- "ReportSuites"
  } else {
    class(r) <- "ReportSuite"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
