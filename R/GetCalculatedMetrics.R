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

#' @export
#' @keywords internal
as.data.frame.CalculatedMetrics <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
CalcMetrics <- function(x) {
  UseMethod("CalculatedMetrics", x)
}

#' @export
#' @keywords internal
as.data.frame.CalculatedMetric <- function(x) {

  if(is.null(x$response$description)){
    x$response$description <- NA
  }

  df <- as.data.frame(list(id = x$response$id,
                           name = x$response$name,
                           description = x$response$description,
                           rsid = x$response$rsid,
                           polarity = x$response$polarity,
                           precision = x$response$precision,
                           type = x$response$type,
                           owner.id = x$response$owner$id)
                      )

  #names(df) <- c("id", "name", "description", "rsid", "polarity", "precision", "type", "owner.id")

  return(df)

}

#' @export
#' @keywords internal
CalcMetric <- function(x) {
  UseMethod("CalculatedMetric", x)
}
