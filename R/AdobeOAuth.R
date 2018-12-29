#' @details Details
#'
#' @description Description
#'
#' @title OAuth2 authentication for Adobe Analytics API
#'
#' @param key key
#' @param secret secret
#' @param globalCompanyId globalCompanyId
#' @param scope scope
#' @param authfile authfile
#'
#' @return Token 2.0 R6 Class (httr)
#' @export
#'
#' @examples
#'
AdobeOAuth <- function(key,
                       secret,
                       globalCompanyId=NULL,
                       scope="openid,AdobeID,read_organizations,additional_info.job_function,additional_info.projectedProductContext",
                       authfile=".httr-oauth") {

  #TODO: Input validation
  #TODO: Check if authfile exists. If so, don't build new one
  #TODO: If authfile exists, test if it works or expired. If expired, refresh

  #ELSE: run build code below
  adobe_endpoints <- httr::oauth_endpoint(
                    authorize = sprintf("https://ims-na1.adobelogin.com/ims/authorize/v1?client_id=%s&scope=%s&response_type=code", key, scope),
                    access = "https://ims-na1.adobelogin.com/ims/token/v1"
                    )

  #TODO: determine if these are always correct or should be function arguments
  auth <- httr::oauth2.0_token(
    endpoint = adobe_endpoints,
    app = httr::oauth_app("adober", key, secret),
    scope = scope,
    use_oob = TRUE,
    cache = TRUE
  )

  #Assign to AdobeRInternals environment, so that other functions know where auth located
  assign("auth", auth, envir = AdobeRInternals)

  #If user-specifies globalCompanyId, then store it
  #Otherwise, get the id by calling GetUserCompanyAccess and take first row
  #Assumption is that first row will be only row for most; if more than one row
  #user should've specified or gets the one automatically chosen
  if(!is.null(globalCompanyId)){
    assign("globalCompanyId", globalCompanyId, envir = AdobeRInternals)
  } else {
    guca <- GetUserCompanyAccess()
    assign("globalCompanyId", guca[1,]$globalCompanyId, envir = AdobeRInternals)
  }

}

#Create an environment to hold credentials
AdobeRInternals <- new.env(parent = emptyenv())
