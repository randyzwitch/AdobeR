#' adobe_get
#'
#' @param key key
#' @param secret secret
#' @param scope scope
#' @param outfile outfile
#'
#' @return Environment
#' @export
#'
#' @examples
#'
adobe_oauth <- function(key,
                        secret,
                        scope="openid,AdobeID,read_organizations,additional_info.job_function,additional_info.projectedProductContext",
                        outfile=".httr-oauth") {

  adobe_endpoints <- oauth_endpoint(authorize = sprintf("https://ims-na1.adobelogin.com/ims/authorize?client_id=%s&scope=%s&response_type=code", key, scope),
                                    access = "https://ims-na1.adobelogin.com/ims/token/v1",
                                    discovery = "https://analytics.adobe.io/discovery/me"
                                    )

  auth <- oauth2.0_token(
    endpoint = adobe_endpoints,
    app = oauth_app("adober", key, secret),
    scope = scope,
    use_oob = TRUE,
    cache = TRUE
  )

  return(auth)
}
