#' Get calculated metrics defined in a given report suite(s)
#'
#' @param id (character) The calculated metric ID to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#' @param rsids (character) Only include calculated metrics tied to specified
#'   RSID list (comma-delimited)
#' @param ownerId (integer) Only include calculated metrics owned by the
#'   specified loginId
#' @param calculatedMetricFilter (character) Only include calculated metrics in
#'   the specified list (comma-delimited list of IDs)
#' @param locale (character) Locale for encoding/localized spelling
#' @param name (character) Only include calculated metrics that contains the Name
#' @param tagNames (character) Only include calculated metrics that contains
#'   one of the tags
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#' @param expansion (character) Comma-delimited list of additional calculated
#'   metric metadata fields to include on response
#'
#' @return data.frame or S3 (CalculatedMetric | CalculatedMetrics)
#' @export
#'
#' @examples
#' \dontrun{
#' cm <- GetCalculatedMetrics()
#' cmid <- GetCalculatedMetrics("cm300005752_557fc500e4b013edc2a85531")
#'
#' #returns S3 CalculatedMetrics
#' cm.nodf <- GetCalculatedMetrics(as.data.frame=FALSE)
#' #returns S3 CalculatedMetric
#' cmid.nodf <- GetCalculatedMetrics("cm300005752_557fc500e4b013edc2a85531", as.data.frame = FALSE)
#'
#' }
GetCalculatedMetrics <- function(id=NULL,
                                 as.data.frame=TRUE,
                                 rsids=NULL,
                                 ownerId=NULL,
                                 calculatedMetricFilter=NULL,
                                 locale=NULL,
                                 name=NULL,
                                 tagNames=NULL,
                                 limit=100,
                                 page=0,
                                 expansion=NULL
                                 ) {

  assertthat::assert_that(is.character(id) || is.null(id),
                          msg="id required to be class 'character'")
  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(rsids) || is.null(rsids),
                          msg="rsids required to be class 'character'")
  assertthat::assert_that(is.character(ownerId) || is.null(ownerId),
                          msg="ownerId required to be class 'character'")
  assertthat::assert_that(is.character(calculatedMetricFilter) || is.null(calculatedMetricFilter),
                          msg="calculatedMetricFilter required to be class 'character'")
  assertthat::assert_that(is.character(locale) || is.null(locale),
                          msg="locale required to be class 'character'")
  assertthat::assert_that(is.character(name) || is.null(name),
                          msg="name required to be class 'character'")
  assertthat::assert_that(is.character(tagNames) || is.null(tagNames),
                          msg="tagNames required to be class 'character'")
  assertthat::assert_that(is.numeric(limit),
                          msg="limit required to be class 'numeric' (integer)")
  assertthat::assert_that(is.numeric(page),
                          msg="page required to be class 'numeric' (integer)")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")


  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s", globalCompanyId)
  resource <- "/calculatedmetrics"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  query <- list(rsids=rsids,
                ownerId=ownerId,
                calculatedMetricFilter=calculatedMetricFilter,
                locale=locale,
                name=name,
                tagNames=tagNames,
                limit=limit,
                page=page,
                expansion=expansion
                )

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

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
