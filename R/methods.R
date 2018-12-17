##### as.data.frame methods for S3 classes that already return df

#' @export
#' @keywords internal
as.data.frame.CalculatedMetrics <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.Dimensions <- function(x) x$response

#' @export
#' @keywords internal
as.data.frame.ReportSuites <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.Segments <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.Tags <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.Users <- function(x) x$response$content

#' @export
#' @keywords internal
as.data.frame.Metrics <- function(x) x$response

#' @export
#' @keywords internal
as.data.frame.DateRanges <- function(x) x$response$content


##### as.data.frame methods for S3 classes that need custom df logic

#' @export
#' @keywords internal
as.data.frame.CalculatedMetric <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("id", "name", "description", "rsid", "polarity",
                 "precision", "type", "owner.id")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.Dimension <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("id", "title", "name", "type", "category", "support", "pathable",
                 "extraTitleInfo", "segmentable", "reportable", "supportsDataGovernance")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.ReportSuite <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("collectionItemType", "rsid")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.Segment <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("id", "name", "description", "rsid", "owner.id")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.Tag <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("id", "name")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.User <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("companyid", "loginId", "login", "createDate", "disabled", "email",
                 "firstName", "fullName", "imsUserId", "lastName", "lastLogin",
                 "lastAccess", "phoneNumber", "tempLoginEnd", "title")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.Metric <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))

  #Build data.frame, set column names so that no surprises occur
  df <- as.data.frame(filled)
  names(df) <- c("id", "title", "name", "type", "category", "support", "allocation",
                 "precision", "calculated", "segmentable", "supportsDataGovernance",
                 "polarity")

  return(df)

}

#' @export
#' @keywords internal
as.data.frame.DateRange <- function(x) {

  #Defensively change any NULL values to NA
  #This is an issue with some methods that silently drop NULL in lists
  filled <- lapply(x$response, function(x) ifelse(is.null(x), "NA", x))
  df <- as.data.frame(filled)

  names(df) <- c("id", "name", "description", "owner.id")
  return(df)

}
