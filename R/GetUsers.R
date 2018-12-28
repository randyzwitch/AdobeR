#' GetUsers
#'
#' @param id (character) ID of user you want to retrieve
#' @param as.data.frame (logical) Return result as data.frame
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
GetUsers <- function(id=NULL, as.data.frame=TRUE) {

  globalCompanyId <- AdobeRInternals$globalCompanyId

  endpoint <- sprintf("https://analytics.adobe.io/api/%s",
                      globalCompanyId)

  resource <- "/users"

  if(!is.null(id)){
    resource <- paste(resource, "/", id, sep="")
  }

  r <- adobe_get(endpoint, resource, globalCompanyId)

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
