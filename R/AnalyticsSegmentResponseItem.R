#' AnalyticsSegmentResponseItem
#'
#'
#'
#'
#'
#' @return AnalyticsSegmentResponseItem
#' @export
#'
#' @examples
#'

AnalyticsSegmentResponseItem <- R6::R6Class(
  "AnalyticsSegmentResponseItem",
  list(
    id = NULL,
    name = NULL,
    description = NULL,
    rsid = NULL,
    reportSuiteName = NULL,
    owner = NULL,
    isDeleted = NULL,
    definition = NULL,
    shares = NULL,
    approved = NULL,
    favorite = NULL,
    usageSummary = NULL,
    siteTitle = NULL,
    tags = NULL,
    modified = NULL,
    created = NULL,

    initialize = function(list) {
      self$id <- list$id
      self$name <- list$name
      self$description <- list$description
      self$rsid <- list$rsid
      self$reportSuiteName <- list$reportSuiteName
      self$owner <- Owner$new(list$owner)
      self$isDeleted <- list$isDeleted
      self$definition <- list$definition  #segment definition
      self$shares <- list$shares  #Array[Share]
      self$approved <- list$approved
      self$favorite <- list$favorite
      self$usageSummary <- SummarizedUsageItem$new(list$usageSummary)
      self$siteTitle <- list$siteTitle
      self$tags <- list$tags  #Array[Tag]
      self$modified <- list$modified
      self$created <- list$created

    }

  )

)
