#' GetUsers
#'
#' @param globalCompanyId
#' @param as.data.frame
#'
#' @return
#' @export
#'
#' @examples
GetUsers <- function(globalCompanyId, id=NULL, as.data.frame=TRUE) {

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/users"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, AdobeRInternals$auth, globalCompanyId)

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

#' @export
#' @keywords internal
as.data.frame.Users <- function(x) {

  return(x$response$content)

}

#' @export
#' @keywords internal
Users <- function(x) {
  UseMethod("Users", x)
}

#' @export
#' @keywords internal
as.data.frame.User <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  # df <- as.data.frame(list(
  #   companyid = filled$companyid,
  #   loginId = filled$loginId,
  #   login = filled$login,
  #   admin = filled$admin,
  #   createDate = filled$createDate,
  #   disabled = filled$disabled,
  #   email = filled$email,
  #   firstName = filled$firstName,
  #   fullName = filled$fullName,
  #   imsUserId = filled$imsUserId,
  #   lastName = filled$lastName,
  #   lastLogin = filled$lastLogin,
  #   lastAccess = filled$lastAccess,
  #   phoneNumber = filled$phoneNumber,
  #   tempLoginEnd = filled$tempLoginEnd,
  #   title = filled$title
  # ))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
User <- function(x) {
  UseMethod("User", x)
}
