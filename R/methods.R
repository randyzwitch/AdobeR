#' @export
#' @keywords internal
as.data.frame.CalculatedMetrics <- function(x) x$response$content

#' @export
#' @keywords internal
CalcMetrics <- function(x) UseMethod("CalculatedMetrics", x)

#' @export
#' @keywords internal
as.data.frame.CalculatedMetric <- function(x) {

  if(is.null(x$response$description)){
    x$response$description <- NA
  }

  df <- as.data.frame(list(id = x$response$id,
                           name = x$response$name,
                           description = x$response$description,
                           rsid = x$response$rsid,
                           polarity = x$response$polarity,
                           precision = x$response$precision,
                           type = x$response$type,
                           owner.id = x$response$owner$id)
  )

  return(df)

}

#' @export
#' @keywords internal
CalcMetric <- function(x) UseMethod("CalculatedMetric", x)

#' @export
#' @keywords internal
as.data.frame.Dimensions <- function(x) return(x$response)

#' @export
#' @keywords internal
Dimensions <- function(x) UseMethod("Dimensions", x)

#' @export
#' @keywords internal
as.data.frame.Dimension <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
Dimension <- function(x) UseMethod("Dimension", x)

#' @export
#' @keywords internal
as.data.frame.ReportSuites <- function(x) x$response$content

#' @export
#' @keywords internal
ReportSuites <- function(x) UseMethod("ReportSuites", x)

#' @export
#' @keywords internal
as.data.frame.ReportSuite <- function(x) {

  df <- as.data.frame(list(collectionItemType = x$response$collectionItemType,
                           rsid = x$response$rsid)
  )

  return(df)

}

#' @export
#' @keywords internal
ReportSuite <- function(x) UseMethod("ReportSuite", x)

#' @export
#' @keywords internal
as.data.frame.Segments <- function(x) x$response$content

#' @export
#' @keywords internal
Segments <- function(x) UseMethod("Segments", x)

#' @export
#' @keywords internal
as.data.frame.Segment <- function(x) {

  df <- as.data.frame(list(
    id = x$response$id,
    name = x$response$name,
    description = x$response$description,
    rsid = x$response$rsid,
    onwer.id = x$response$owner$id
  ))

  return(df)

}

#' @export
#' @keywords internal
Segment <- function(x) UseMethod("Segment", x)

#' @export
#' @keywords internal
as.data.frame.Tags <- function(x) return(x$response$content)

#' @export
#' @keywords internal
CreateTags <- function(x) UseMethod("Tags", x)

#' @export
#' @keywords internal
as.data.frame.Tag <- function(x) {

  df <- as.data.frame(list(id = x$response$id,
                           name = x$response$name))
  return(df)

}

#' @export
#' @keywords internal
CreateTag <- function(x) UseMethod("Tag", x)

#' @export
#' @keywords internal
CompanyAccess <- function(x) UseMethod("CompanyAccess", x)

#' @export
#' @keywords internal
as.data.frame.Users <- function(x) x$response$content

#' @export
#' @keywords internal
Users <- function(x) UseMethod("Users", x)

#' @export
#' @keywords internal
as.data.frame.User <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
User <- function(x) UseMethod("User", x)

#' @export
#' @keywords internal
as.data.frame.Metrics <- function(x) x$response

#' @export
#' @keywords internal
Metrics <- function(x) UseMethod("Metrics", x)

#' @export
#' @keywords internal
as.data.frame.Metric <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
Metric <- function(x) UseMethod("Metric", x)

#' @export
#' @keywords internal
as.data.frame.DateRanges <- function(x) x$response$content

#' @export
#' @keywords internal
DateRanges <- function(x) UseMethod("DateRanges", x)

#' @export
#' @keywords internal
as.data.frame.DateRange <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))
  df <- as.data.frame(filled)

  names(df) <- c("id", "name", "description", "owner.id")
  return(df)

}

#' @export
#' @keywords internal
DateRange <- function(x) UseMethod("DateRange", x)
