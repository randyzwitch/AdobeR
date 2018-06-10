#' Tag
#'
#'
#'
#'
#'
#' @return Tag
#' @export
#'
#' @examples
#'

Tag <- R6::R6Class(
  "Tag",
  list(
    id = NULL,
    name = NULL,
    description = NULL,
    components = NULL,

    initialize = function(list) {
      self$id <- list$id
      self$name <- list$name
      self$description <- list$description
      self$components <- list$components

    }

  )

)
