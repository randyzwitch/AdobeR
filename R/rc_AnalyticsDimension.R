#' AnalyticsDimension
#'
#'
#'
#'
#'
#' @return AnalyticsDimension
#' @export
#'
#' @examples
#'

AnalyticsDimension <- R6::R6Class(
  "AnalyticsDimension",
  list(
    id = NULL,
    title = NULL,
    name = NULL,
    type = NULL,
    category = NULL,
    parent = NULL,
    extraTitleInfo = NULL,
    description = NULL,
    hidden = NULL,
    noAccess = NULL,
    shares = NULL,
    approved = NULL,
    favorite = NULL,
    usageSummary = NULL,
    tags = NULL,

    initialize = function(list) {
      self$id <- list$id
      self$title <- list$id
      self$name <- list$id
      self$type <- list$id
      self$category <- list$id
      self$parent <- list$id
      self$extraTitleInfo <- list$id
      self$description <- list$id
      self$hidden <- list$id
      self$noAccess <- list$id
      self$shares <- list$id  #Array[Share]
      self$approved <- list$id
      self$favorite <- list$id
      self$usageSummary <- SummarizedUsageItem$new(list$id)
      self$tags <- list$id  #Array[Tag]

    }

  )

)
