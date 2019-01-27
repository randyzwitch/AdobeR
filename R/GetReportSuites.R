#' Get report suites for a company account
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
#' @param rsids (character) Only include suites in this RSID list
#'   (comma-delimited)
#' @param rsidContains (character) Only include suites whose rsid contains
#'   rsidContains
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
                            expansion=NULL) {

  assertthat::assert_that(is.character(rsid) || is.null(rsid),
                          msg="rsid required to be class 'character'")
  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.character(rsids) || is.null(rsids),
                          msg="rsids required to be class 'character'")
  assertthat::assert_that(is.character(rsidContains) || is.null(rsidContains),
                          msg="rsidContains required to be class 'character'")
  assertthat::assert_that(is.character(expansion) || is.null(expansion),
                          msg="expansion required to be class 'character'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)
  resource <- "/collections/suites"

  limit=100
  page=0
  r <- list()

  repeat{

    query <- list(rsids=rsids,
                  rsidContains=rsidContains,
                  limit=limit,
                  page=page,
                  expansion=expansion
                  )

    tmp <- adobe_get(endpoint, resource, globalCompanyId, query)

    #Set S3 method for easier parsing later
    class(tmp) <- append(class(tmp), "ReportSuites")

    r <- append(r, list(tmp))
    page <- page + 1

    if(tmp$response$lastPage){
      break
    }

  }

  class(r) <- "ReportSuitesList"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(dplyr::bind_rows(lapply(r, as.data.frame)))
  }

  return(r)
}
