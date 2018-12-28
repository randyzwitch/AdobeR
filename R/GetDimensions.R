#' GetDimensions
#'
#' @param rsid (character) The report suite ID
#' @param dimension (character) The dimension ID
#' @param as.data.frame (logical) Return result as data.frame
#'
#' @return data.frame or S3 (Dimensions | Dimension)
#' @export
#'
#' @examples
#' \dontrun{
#' dims <- GetDimensions("zwitchdev")
#' dims.nodf <- GetDimensions("zwitchdev", as.data.frame = FALSE)
#' dimsevar1 <- GetDimensions("zwitchdev", "evar1")
#' dimsevar1.nodf <- GetDimensions("zwitchdev", "evar1", as.data.frame = FALSE)
#'
#' }
GetDimensions <- function(rsid, dimension=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/dimensions",
                      globalCompanyId)
  resource <- paste("?rsid=", rsid, sep="")

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
