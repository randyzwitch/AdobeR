#' GetReportSuites
#'
#' @param globalCompanyId
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetReportSuites <- function(globalCompanyId, rsid=NULL, as.data.frame=TRUE) {

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

#' @export
#' @keywords internal
as.data.frame.ReportSuites <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
ReportSuites <- function(x) {
  UseMethod("ReportSuites", x)
}

#' @export
#' @keywords internal
as.data.frame.ReportSuite <- function(x) {

  df <- as.data.frame(list(collectionItemType = x$response$collectionItemType,
                           rsid = x$response$rsid)
                      )

  return(df)

}

#' @export
#' @keywords internal
ReportSuite <- function(x) {
  UseMethod("ReportSuite", x)
}
