#' GetSegments
#'
#' @param globalCompanyId
#' @param id
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetSegments <- function(globalCompanyId, id=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/segments"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

  #Set S3 method for easier parsing later
  if(is.null(id)){
    class(r) <- "Segments"
  } else {
    class(r) <- "Segment"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}

#' @export
#' @keywords internal
as.data.frame.Segments <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
Segments <- function(x) {
  UseMethod("Segments", x)
}

#' @export
#' @keywords internal
as.data.frame.Segment <- function(x) {

  df <- as.data.frame(list(
    id = x$response$id,
    name = x$response$name,
    description = x$response$description,
    rsid = x$response$rsid,
    onwer.id = x$response$owner$id
  ))

  return(df)

}

#' @export
#' @keywords internal
Segment <- function(x) {
  UseMethod("Segment", x)
}
