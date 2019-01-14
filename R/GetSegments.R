#' Get user-defined segments for a company account
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
#'
#' @param as.data.frame (logical) Return result as data.frame
#' @param rsids (character) Only include segments tied to specified RSID list
#'   (comma-delimited)
#' @param segmentFilter (character) Only include segments in the specified list
#'   (comma-delimited list of IDs)
#' @param locale (character) Locale for encoding/localized spelling
#' @param name (character) Only include segments that contains the name
#' @param tagNames (character) Only include segments that contains one of the tags
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#' @param expansion (character) Comma-delimited list of additional metadata
#'   fields to include on response
#'
#' @return data.frame or S3 'Segments'
#' @export
#'
#' @examples
#' \dontrun{
#' seg <- GetSegments()
#' seg.nodf <- GetSegments(as.data.frame = FALSE)
#'
#' }
GetSegments <- function(as.data.frame=TRUE,
                        rsids=NULL,
                        segmentFilter=NULL,
                        locale=NULL,
                        name=NULL,
                        tagNames=NULL,
                        limit=100,
                        page=0,
                        expansion=NULL) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(rsids) || is.null(rsids),
                          msg="rsids required to be class 'character'")
  assertthat::assert_that(is.character(segmentFilter) || is.null(segmentFilter),
                          msg="segmentFilter required to be class 'character'")
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

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/segments"

  query <- list(rsids=rsids,
                segmentFilter=segmentFilter,
                locale=locale,
                name=name,
                tagNames=tagNames,
                limit=limit,
                page=page,
                expansion=expansion)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "Segments")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
