#' Get users for a company account
#'
#' @param as.data.frame (logical) Return result as data.frame
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
GetUsers <- function(as.data.frame=TRUE) {

  assertthat::assert_that(is.logical(as.data.frame),
                          msg="as.data.frame required to be class 'logical'")

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/users"

  limit=100
  page=0
  r <- list()

  repeat{

    query <- list(limit=limit, page=page)

    tmp <- adobe_get(endpoint, resource, globalCompanyId, query)

    #Set S3 method for easier parsing later
    class(tmp) <- append(class(tmp), "Users")

    r <- append(r, list(tmp))
    page <- page + 1

    if(tmp$response$lastPage){
      break
    }

  }

  class(r) <- "UsersList"

  #Return a data.frame or just an S3 object
  if(as.data.frame){
    return(dplyr::bind_rows(lapply(r, as.data.frame)))
  }

  return(r)
}
