#' CalendarType
#'
#'
#'
#'
#'
#' @return CalendarType
#' @export
#'
#' @examples
#'

CalendarType <- R6::R6Class(
  "CalendarType",
  list(
    rsid = NULL,
    type = NULL,
    anchorDate = NULL,

    initialize = function(list) {
      self$rsid <- list$rsid
      self$type <- list$type
      self$anchorDate <- list$anchorDate

    }

  )

)
