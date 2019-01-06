#' Get users for a company account
#'
#' @param as.data.frame (logical) Return result as data.frame
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#'
#' @return data.frame or S3 'Users'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' users <- GetUsers()
#' users.nodf <- GetUsers(as.data.frame = FALSE) #returns S3 Users
#'
#' }
GetUsers <- function(as.data.frame=TRUE,
                     limit=100,
                     page=0) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")
  assertthat::assert_that(is.numeric(limit),
                          msg="limit required to be class 'numeric' (integer)")
  assertthat::assert_that(is.numeric(page),
                          msg="page required to be class 'numeric' (integer)")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/users"

  query <- list(limit=limit, page=page)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  class(r) <- append(class(r), "Users")

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
