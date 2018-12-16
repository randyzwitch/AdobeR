#' GetDimensions
#'
#' @param globalCompanyId
#' @param rsid
#' @param dimension
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetDimensions <- function(globalCompanyId, rsid, dimension=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/dimensions",
                      globalCompanyId)
  resource <- paste("?rsid=", rsid, "&classifiable=false", sep="")

  if(!is.null(dimension)){
    endpoint <- paste(endpoint, "/", dimension, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(dimension)){
    class(r) <- "Dimensions"
  } else {
    class(r) <- "Dimension"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}

#' @export
#' @keywords internal
as.data.frame.Dimensions <- function(x) {

  return(x$response)

}

#' @export
#' @keywords internal
Dimensions <- function(x) {
  UseMethod("Dimensions", x)
}

#' @export
#' @keywords internal
as.data.frame.Dimension <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
Dimension <- function(x) {
  UseMethod("Dimension", x)
}
