#' AnalyticsCalculatedMetric
#'
#'
#'
#'
#'
#' @return AnalyticsCalculatedMetric
#' @export
#'
#' @examples
#'

AnalyticsCalculatedMetric <- R6::R6Class(
  "AnalyticsCalculatedMetric",
  list(
    id = NULL,
    name = NULL,
    description = NULL,
    rsid = NULL,
    reportSuiteName = NULL,
    owner = NULL,
    isDeleted = NULL,
    polarity = NULL,
    precision = NULL,
    type = NULL,
    definition = NULL,
    template = NULL,
    shares = NULL,
    approved = NULL,
    favorite = NULL,
    usageSummary = NULL,
    tags = NULL,
    siteTitle = NULL,
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
      self$polarity <- list$polarity
      self$precision <- list$precision
      self$type <- list$type
      self$definition <- list$definition  #CalculatedMetricDef
      self$template <- list$template
      self$shares <- list$shares  #Array[Share]
      self$approved <- list$approved
      self$favorite <- list$favorite
      self$usageSummary <- SummarizedUsageItem$new(list$usageSummary)
      self$tags <- list$tags  #Array[Tag]
      self$siteTitle <- list$siteTitle
      self$modified <- list$modified
      self$created <- list$created

    }

  )

)
