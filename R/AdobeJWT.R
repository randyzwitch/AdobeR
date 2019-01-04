#' Title
#'
#' @param privatekey
#' @param orgid
#' @param techacctid
#' @param clientid
#' @param secret
#'
#' @return
#' @export
#'
#' @examples
AdobeJWT <- function(privatekey,
                     orgid,
                     techacctid,
                     clientid,
                     secret,
                     globalCompanyId=NULL){

  #Need to collapse using paste as readLines will split on newline
  #which makes a list instead of a string
  pkey <- paste(readLines("private.key"), collapse = "\n")

  #Paste in information from JWT section of Adobe website
  claim <- jwt_claim(exp=as.integer(Sys.time() + 3600 * 24),
                     iss=orgid,
                     sub=techacctid,
                     aud=paste("https://ims-na1.adobelogin.com/c/", clientid, sep=""),
                     "https://ims-na1.adobelogin.com/s/ent_analytics_bulk_ingest_sdk"=TRUE
                     )

  #Create JWT
  val <- jwt_encode_sig(claim = claim, pkey, size = 256)

  #make POST call to swap JWT for bearer token
  token <- POST("https://ims-na1.adobelogin.com/ims/exchange/jwt/",
                encode="form",
                body = list(client_id=clientid,
                            client_secret=secret,
                            jwt_token=val)
  )

  #get content as R object instead of binary
  cont <- content(token)

  auth <- list(credentials=cont, app=list(key=clientid))

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
