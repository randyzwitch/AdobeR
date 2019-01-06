#' Get dimensions (props, eVars, etc.) defined in a given report suite(s)
#'
#' @param rsid (character) The report suite ID
#' @param as.data.frame (logical) Return result as data.frame
#' @param locale (character) Locale for encoding/localized spelling
#' @param segmentable (logical) Only include dimensions that are valid within a
#'   segment
#' @param reportable (logical) Only include dimensions that are valid within a
#'   report
#' @param classifiable (logical) Only include classifiable dimensions
#' @param expansion (character) Add extra metadata to items (comma-delimited list)
#'
#' @return data.frame or S3 'Dimensions'
#' @export
#'
#' @examples
#' \dontrun{
#' dims <- GetDimensions("zwitchdev")
#' dims.nodf <- GetDimensions("zwitchdev", as.data.frame = FALSE)
#'
#' }
GetDimensions <- function(rsid,
                          as.data.frame=TRUE,
                          locale=NULL,
                          segmentable=NULL,
                          reportable=NULL,
                          classifiable=NULL,
                          expansion=NULL
                          ) {

  assertthat::assert_that(is.character(rsid),
                          msg="rsid required to be class 'character'")
  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(locale) || is.null(locale),
                          msg="locale required to be class 'character'")
  assertthat::assert_that(is.logical(segmentable) || is.null(segmentable),
                          msg="segmentable required to be class 'logical'")
  assertthat::assert_that(is.logical(reportable) || is.null(reportable),
                          msg="reportable required to be class 'logical'")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/dimensions",
                      globalCompanyId)

  #resource can stay empty, as dimensions built into endpoint string
  resource <- ""

  #pass query parameters as named list to httr rather than
  #build string using paste
  query <- list(rsid=rsid,
                locale=locale,
                segmentable=segmentable,
                reportable=reportable,
                classifiable=classifiable,
                expansion=expansion)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "Dimensions")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
