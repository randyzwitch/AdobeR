#' TaggedComponent
#'
#'
#'
#'
#'
#' @return TaggedComponent
#' @export
#'
#' @examples
#'

TaggedComponent <- R6::R6Class(
  "TaggedComponent",
  list(
    componentType = NULL,
    componentId = NULL,
    tags = NULL,

    initialize = function(list) {
      self$componentType <- list$componentType
      self$componentId <- list$componentId
      self$tags <- list$tags  #Array[Tag], come back and lapply Tag()

    }

  )

)
