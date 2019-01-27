#' Get user-defined date ranges for a company account
#'
#' @details The 'expansion' keyword allows for specifying a list of additional
#'   fields to return as part of the response. These fields should be specified
#'   as a character vector (string), separated with commas and no spaces between
#'   the fields.
#'
#'   Because of the complex nature of having unknown combinations of extra
#'   fields returned on a function call, using the 'expansion' field with
#'   as.data.frame=TRUE can return undesirable results.
#'
#' @param as.data.frame (logical) Return result as data.frame
#' @param locale (character) Locale for encoding/localized spelling
#' @param filterByIds (character) Only include date ranges in the specified
#'   list (comma-delimited list of IDs)
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
                          expansion=NULL) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(locale) || is.null(locale),
                          msg="locale required to be class 'character'")
  assertthat::assert_that(is.character(filterByIds) || is.null(filterByIds),
                          msg="filterByIds required to be class 'character'")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")


  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s", globalCompanyId)

  resource <- "/dateranges"

  limit=100
  page=0
  r <- list()

  repeat{

    query <- list(locale=locale,
                  filterByIds=filterByIds,
                  limit=limit,
                  page=page,
                  expansion=expansion)

    tmp <- adobe_get(endpoint, resource, globalCompanyId, query)

    #Set S3 method for easier parsing later
    class(tmp) <- append(class(tmp), "DateRanges")

    r <- append(r, list(tmp))
    page <- page + 1

    if(tmp$response$lastPage){
      break
    }

  }

  class(r) <- "DateRangesList"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
