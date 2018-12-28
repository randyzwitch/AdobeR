#' GetReportSuites
#'
#' @param rsid (character) ID of desired report suite
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (ReportSuites | ReportSuite)
#' @export
#'
#' @examples
#' \dontrun{
#' grs <- GetReportSuites()
#' grs.nodf <- GetReportSuites(as.data.frame = FALSE)
#' grsrsid <- GetReportSuites("zwitchdev")
#' grsrsid.nodf <- GetReportSuites("zwitchdev", as.data.frame = FALSE)
#'
#' }
GetReportSuites <- function(rsid=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/collections/suites"

  if(!is.null(rsid)){
    resource <- paste(resource, "/", rsid, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

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
