#' OAuth2 web authentication
#'
#' @details For user convenience, this method calls GetUserCompanyAccess() after
#'   doing the 3-legged OAuth authentication procedure in order to get the
#'   globalCompanyId. As part of this convenience, the method assumes that a user
#'   only has access to a single company.
#'
#'   If your email address has access to
#'   multiple Adobe Analytics accounts AND the company you are interested in
#'   accessing is not listed first in the GetUserCompanyAccess() response, you
#'   can set the globalCompanyId in the AdobeOAuth function to the proper value.
#'
#' @description Authenticates with Adobe Analytics API using pre-defined
#'   OAuth integration at https://console.adobe.io. This style of authentication
#'   requires the user to copy an authentication code and input it into the R
#'   console, and as such, is not appropriate for automation or service accounts.
#'
#' @param key (character) API Key (Client ID) from adobe.io console
#' @param secret (character) Client secret from adobe.io console
#' @param globalCompanyId (character) Desired globalCompanyId (defined by Adobe)
#'   to access
#' @param scope (character) Access scopes for account
#' @param authfile (character) File name for cached OAuth credentials
#'
#' @seealso \code{\link{GetUserCompanyAccess}}
#'
#' @return Token 2.0 R6 Class (httr) in 'AdobeRInternals' environment (hidden)
#' @export
#'
#' @examples
#' \dontrun{
#'
#' # get this from the adobe.io console
#' key <- "e6e798e57ba67330b42061e722c420cb"
#' secret <- "427bc850-714c-4bcg-857b-5b74e11206c6"
#'
#' # This triggers the OAuth 3-legged authentication via browser
#' # A browser window will open to sign into Adobe, which will then re-direct
#' # to url specified during integration registration step
#' AdobeOAuth(key, secret)
#'
#' }
#'
AdobeOAuth <- function(key,
                       secret,
                       globalCompanyId=NULL,
                       scope=NULL,
                       authfile=".httr-oauth") {

  assertthat::assert_that(is.character(key),
                          msg="key required to be class 'character'")
  assertthat::assert_that(is.character(secret),
                          msg="secret required to be class 'character'")
  assertthat::assert_that(is.character(globalCompanyId) || is.null(globalCompanyId),
                          msg="globalCompanyId required to be class 'character'")
  assertthat::assert_that(is.character(scope) || is.null(scope),
                          msg="scope required to be class 'character'")

  #If user doesn't specify, make scope wide as possible
  if(is.null(scope)){
    scope <- "openid,AdobeID,read_organizations,additional_info.job_function,additional_info.projectedProductContext"
  }

  adobe_endpoints <- httr::oauth_endpoint(
                    authorize = sprintf("https://ims-na1.adobelogin.com/ims/authorize/v1?client_id=%s&scope=%s&response_type=code", key, scope),
                    access = "https://ims-na1.adobelogin.com/ims/token/"
                    )

  #If .httr-oauth exists and key/secret hash in file, will skip the web auth
  #Process doesn't check for valid token though
  auth <- httr::oauth2.0_token(
    endpoint = adobe_endpoints,
    app = httr::oauth_app("adober", key, secret),
    scope = scope,
    use_oob = TRUE,
    cache = TRUE
  )

  #Assign to AdobeRInternals environment, so that other functions know where auth located
  assign("auth", auth, envir = AdobeRInternals)

  #Make call to validate whether token valid
  guca <- GetUserCompanyAccess(FALSE)
  ifelse("AdobeRSuccess" %in% class(guca),
         print("Token valid"),
         print("Token expired, refresh")
  )

  #authDEV <<- AdobeRInternals$auth

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
