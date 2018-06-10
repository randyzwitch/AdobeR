#' AnalyticsUser
#'
#'
#'
#'
#'
#' @return AnalyticsUser
#' @export
#'
#' @examples
#'

AnalyticsUser <- R6::R6Class(
  "AnalyticsUser",
  list(
    companyid = NULL,
    loginId = NULL,
    login = NULL,
    admin = NULL,
    apiKey = NULL,
    createDate = NULL,
    disabled = NULL,
    email = NULL,
    firstName = NULL,
    fullName = NULL,
    imsUserId = NULL,
    lastName = NULL,
    lastLogin = NULL,
    phoneNumber = NULL,
    title = NULL,

    initialize = function(list) {
      self$companyid <- list$companyid
      self$loginId <- list$loginId
      self$login <- list$login
      self$admin <- list$admin
      self$apiKey <- list$apiKey
      self$createDate <- list$createDate
      self$disabled <- list$disabled
      self$email <- list$email
      self$firstName <- list$firstName
      self$fullName <- list$fullName
      self$imsUserId <- list$imsUserId
      self$lastName <- list$lastName
      self$lastLogin <- list$lastLogin
      self$phoneNumber <- list$phoneNumber
      self$title <- list$title
    }

  )

)
