#' @export
#' @keywords internal
as.data.frame.CalculatedMetrics <- function(x) x$response$content

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
as.data.frame.Dimensions <- function(x) return(x$response)

#' @export
#' @keywords internal
as.data.frame.Dimension <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
as.data.frame.ReportSuites <- function(x) x$response$content

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
as.data.frame.Segments <- function(x) x$response$content

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
as.data.frame.Tags <- function(x) return(x$response$content)

#' @export
#' @keywords internal
as.data.frame.Tag <- function(x) {

  df <- as.data.frame(list(id = x$response$id,
                           name = x$response$name))
  return(df)

}

#' @export
#' @keywords internal
as.data.frame.Users <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.User <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
as.data.frame.Metrics <- function(x) x$response

#' @export
#' @keywords internal
as.data.frame.Metric <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  return(as.data.frame(filled))

}

#' @export
#' @keywords internal
as.data.frame.DateRanges <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.DateRange <- function(x) {

  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))
  df <- as.data.frame(filled)

  names(df) <- c("id", "name", "description", "owner.id")
  return(df)

}
