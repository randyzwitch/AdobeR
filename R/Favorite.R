#' Favorite
#'
#'
#'
#'
#'
#' @return Favorite
#' @export
#'
#' @examples
#'

Favorite <- R6::R6Class(
  "Favorite",
  list(
    favoriteId = NULL,
    companyId = NULL,
    userId = NULL,
    componentTypeId = NULL,
    componentType = NULL,
    componentId = NULL,
    favoriteTimestamp = NULL,

    initialize = function(list) {
      self$favoriteId <- list$favoriteId
      self$companyId <- list$companyId
      self$userId <- list$userId
      self$componentTypeId <- list$componentTypeId
      self$componentType <- list$componentType
      self$componentId <- list$componentId
      self$favoriteTimestamp <- list$favoriteTimestamp

    }

  )

)
