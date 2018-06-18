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
    response = NULL,
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
      self$response <- list$response
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
    },
    write_json = function(path) {
      # uses R write method since response is already json
      write(self$response, path)

    },
    as.data.frame = function() {
      # set columns and types up front, so rbind.fill has a template
      # hopefully provides more control about column types as well
      df <- data.frame(
        companyid = integer(),
        loginId = integer(),
        login = character(),
        admin = logical(),
        apiKey = character(),
        createDate = character(),
        disabled = logical(),
        email = character(),
        firstName = character(),
        fullName = character(),
        imsUserId = character(),
        lastName = character(),
        lastLogin = character(),
        phoneNumber = character(),
        title = character(),
        stringsAsFactors = FALSE
      )

      df <- plyr::rbind.fill(
        df,
        as.data.frame(cbind(
          companyid = self$companyid,
          loginId = self$loginId,
          login = self$login,
          admin = self$admin,
          apiKey = self$apiKey,
          createDate = self$createDate,
          disabled = self$disabled,
          email = self$email,
          firstName = self$firstName,
          fullName = self$fullName,
          imsUserId = self$imsUserId,
          lastName = self$lastName,
          lastLogin = self$lastLogin,
          phoneNumber = self$phoneNumber,
          title = self$title
        )
      ))

      return(df)

    }

  )

)
