#' SuiteCollectionItem
#'
#'
#'
#'
#'
#' @return SuiteCollectionItem
#' @export
#'
#' @examples
#'

SuiteCollectionItem <- R6::R6Class(
  "SuiteCollectionItem",
  list(
    name = NULL,
    calendarType = NULL,
    currency = NULL,
    collectionItemType = NULL,
    currentTimezoneOffset = NULL,
    enabledSolutions = NULL,
    dataCurrentAsOf = NULL,
    timezoneZoneinfo = NULL,
    parentRsid = NULL,
    isBlocked = NULL,
    numGroups = NULL,
    isDeleted = NULL,
    rsid = NULL,

    initialize = function(list) {
      self$name <- list$name
      self$calendarType <- CalendarType$new(list$calendarType)
      self$currency <- list$currency
      self$collectionItemType <- list$collectionItemType
      self$currentTimezoneOffset <- list$currentTimezoneOffset
      self$enabledSolutions <- list$enabledSolutions
      self$dataCurrentAsOf <- list$dataCurrentAsOf
      self$timezoneZoneinfo <- list$timezoneZoneinfo
      self$parentRsid <- list$parentRsid
      self$isBlocked <- list$isBlocked
      self$numGroups <- list$numGroups
      self$isDeleted <- list$isDeleted
      self$rsid <- list$rsid

    }

  )
)
