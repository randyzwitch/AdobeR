#' Get user-defined date ranges for a company account
#'
#' @param as.data.frame (logical) Return result as data.frame
#' @param locale (character) Locale for encoding/localized spelling
#' @param filterByIds (character) Only include date ranges in the specified
#'   list (comma-delimited list of IDs)
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#' @param expansion (character) Comma-delimited list of additional date range
#'   metadata fields to include on response
#'
#' @return data.frame or S3 'DateRanges'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' gdr <- GetDateRanges()
#' gdr.nodf <- GetDateRanges(as.data.frame = FALSE)
#'
#' }
GetDateRanges <- function(as.data.frame=TRUE,
                          locale=NULL,
                          filterByIds=NULL,
                          limit=100,
                          page=0,
                          expansion=NULL) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(locale) || is.null(locale),
                          msg="locale required to be class 'character'")
  assertthat::assert_that(is.character(filterByIds) || is.null(filterByIds),
                          msg="filterByIds required to be class 'character'")
  assertthat::assert_that(is.numeric(limit),
                          msg="limit required to be class 'numeric' (integer)")
  assertthat::assert_that(is.numeric(page),
                          msg="page required to be class 'numeric' (integer)")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")


  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s", globalCompanyId)

  resource <- "/dateranges"

  query <- list(locale=locale,
                filterByIds=filterByIds,
                limit=limit,
                page=page,
                expansion=expansion)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "DateRanges")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
