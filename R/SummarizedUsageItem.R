#' SummarizedUsageItem
#'
#'
#'
#'
#'
#' @return SummarizedUsageItem
#' @export
#'
#' @examples
#'

SummarizedUsageItem <- R6::R6Class(
  "SummarizedUsageItem",
  list(
    count = NULL,
    mostRecentTimestamp = NULL,
    itemId = NULL,
    relevancyScore = NULL,

    initialize = function(list) {
      self$count <- list$count
      self$mostRecentTimestamp <- list$mostRecentTimestamp
      self$itemId <- list$itemId
      self$relevancyScore <- list$relevancyScore

    }

  )

)
