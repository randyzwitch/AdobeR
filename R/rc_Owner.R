#' Owner
#'
#'
#'
#'
#'
#' @return Owner
#' @export
#'
#' @examples
#'

Owner <- R6::R6Class(
  "Owner",
  list(
    id = NULL,
    name = NULL,
    login = NULL,

    initialize = function(list) {
      self$id <- list$id
      self$name <- list$name
      self$login <- list$login

    }

  )

)
