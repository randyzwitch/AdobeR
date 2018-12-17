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

#' @export
#' @keywords internal
as.data.frame.DateRanges <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
DateRanges <- function(x) {
  UseMethod("DateRanges", x)
}

#' @export
#' @keywords internal
as.data.frame.DateRange <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))
  df <- as.data.frame(filled)

  names(df) <- c("id", "name", "description", "owner.id")
  return(df)

}

#' @export
#' @keywords internal
DateRange <- function(x) {
  UseMethod("DateRange", x)
}
