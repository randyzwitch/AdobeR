#' Share
#'
#'
#'
#'
#'
#' @return Share
#' @export
#'
#' @examples
#'

Share <- R6::R6Class(
  "Share",
  list(
    shareId = NULL,
    shareToId = NULL,
    shareToType = NULL,
    componentType = NULL,
    componentId = NULL,
    shareToDisplayName = NULL,
    shareToLogin = NULL,

    initialize = function(list) {
      self$shareId <- list$shareId
      self$shareToId <- list$shareToId
      self$shareToType <- list$shareToType
      self$componentType <- list$componentType
      self$componentId <- list$componentId
      self$shareToDisplayName <- list$shareToDisplayName
      self$shareToLogin <- list$shareToLogin

    }

  )

)
