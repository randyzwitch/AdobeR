#' Get metrics (custom events and pre-defined) defined in a given report suite(s)
#'
#' @param rsid (character) ID of desired report suite
#' @param as.data.frame (logical) Return result as data.frame
#' @param locale (character) Locale for encoding/localized spelling
#' @param segmentable (logical) Filter the metrics by if they are valid in a segment
#' @param expansion (character) Add extra metadata to items (comma-delimited list)
#'
#' @return data.frame or S3 'Metrics'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' metrics <- GetMetrics("zwitchdev")
#' metrics.nodf <- GetMetrics("zwitchdev", as.data.frame = FALSE)
#'
#' }
GetMetrics <- function(rsid,
                       as.data.frame=TRUE,
                       locale=NULL,
                       segmentable=NULL,
                       expansion=NULL) {

  assertthat::assert_that(is.character(rsid),
                          msg="rsid required to be class 'character'")
  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(locale) || is.null(locale),
                          msg="locale required to be class 'character'")
  assertthat::assert_that(is.logical(segmentable) || is.null(segmentable),
                          msg="segmentable required to be class 'logical'")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s/metrics",
                      globalCompanyId)

  #resource can stay empty, as dimensions built into endpoint string
  resource <- ""

  #pass query parameters as named list to httr rather than
  #build string using paste
  query <- list(rsid=rsid,
                locale=locale,
                segmentable=segmentable,
                expansion=expansion
                )

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "Metrics")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
