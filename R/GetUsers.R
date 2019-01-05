#' Get users for a company account
#'
#' @param id (character) ID of user you want to retrieve
#' @param as.data.frame (logical) Return result as data.frame
#' @param limit (integer) Number of results per page
#' @param page (integer) Page number (base 0 - first page is "0")
#'
#' @return data.frame or S3 (Users | User)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' users <- GetUsers()
#' users.nodf <- GetUsers(as.data.frame = FALSE) #returns S3 Users
#' userme <- GetUsers("me")
#' userme.nodf <- GetUsers("me", as.data.frame = FALSE)
#'
#' }
GetUsers <- function(id=NULL,
                     as.data.frame=TRUE,
                     limit=100,
                     page=0) {

  assertthat::assert_that(is.character(id) || is.null(id),
                          msg="id required to be class 'character'")
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

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  query <- list(limit=limit, page=page)

  r <- adobe_get(endpoint, resource, globalCompanyId, query)

  #Set S3 method for easier parsing later
  if(is.null(id)){
    class(r) <- "Users"
  } else {
    class(r) <- "User"
  }

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(as.data.frame(r))
  }

  return(r)
}
