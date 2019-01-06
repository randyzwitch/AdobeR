#' Get report suites for a company account
#'
#' @param as.data.frame (logical) Return result as data.frame
#' @param rsids (character) Only include suites in this RSID list
#'   (comma-delimited)
#' @param rsidContains (character) Only include suites whose rsid contains
#'   rsidContains
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#' @param expansion (character) Comma-delimited list of additional metadata
#'   fields to include on response
#'
#' @return data.frame or S3 'ReportSuites'
#' @export
#'
#' @examples
#' \dontrun{
#' grs <- GetReportSuites()
#' grs.nodf <- GetReportSuites(as.data.frame = FALSE)
#'
#' }
GetReportSuites <- function(rsid=NULL,
                            as.data.frame=TRUE,
                            rsids=NULL,
                            rsidContains=NULL,
                            limit=100,
                            page=0,
                            expansion=NULL) {

  assertthat::assert_that(is.character(rsid) || is.null(rsid),
                          msg="rsid required to be class 'character'")
  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(rsids) || is.null(rsids),
                          msg="rsids required to be class 'character'")
  assertthat::assert_that(is.character(rsidContains) || is.null(rsidContains),
                          msg="rsidContains required to be class 'character'")
  assertthat::assert_that(is.numeric(limit),
                          msg="limit required to be class 'numeric' (integer)")
  assertthat::assert_that(is.numeric(page),
                          msg="page required to be class 'numeric' (integer)")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/collections/suites"

  query <- list(rsids=rsids,
                rsidContains=rsidContains,
                limit=limit,
                page=page,
                expansion=expansion
                )

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "ReportSuites")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
