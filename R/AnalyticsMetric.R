#' AnalyticsMetric
#'
#'
#'
#'
#'
#' @return AnalyticsMetric
#' @export
#'
#' @examples
#'

AnalyticsMetric <- R6::R6Class(
  "AnalyticsMetric",
  list(
    id = NULL,
    title = NULL,
    name = NULL,
    type = NULL,
    extraTitleInfo = NULL,
    category = NULL,
    precision = NULL,
    calculated = NULL,
    description = NULL,
    polarity = NULL,
    hidden = NULL,
    shares = NULL,
    approved = NULL,
    favorite = NULL,
    usageSummary = NULL,
    tags = NULL,

    initialize = function(list) {
      self$id <- list$id
      self$title <- list$title
      self$name <- list$name
      self$type <- list$type
      self$extraTitleInfo <- list$extraTitleInfo
      self$category <- list$category
      self$precision <- list$precision
      self$calculated <- list$calculated
      self$description <- list$description
      self$polarity <- list$polarity
      self$hidden <- list$hidden
      self$shares <- list$shares  #Array[Share]
      self$approved <- list$approved
      self$favorite <- list$favorite
      self$usageSummary <- SummarizedUsageItem$new(list$usageSummary)
      self$tags <- list$tags  #Array[Tag]

    }

  )

)
