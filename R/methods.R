##### as.data.frame methods for S3 classes that already return df

#' @export
#' @keywords internal
as.data.frame.CalculatedMetrics <- function(x, ...) x$response$content

#' @export
#' @keywords internal
as.data.frame.Dimensions <- function(x, ...) x$response

#' @export
#' @keywords internal
as.data.frame.ReportSuites <- function(x, ...) x$response$content

#' @export
#' @keywords internal
as.data.frame.Segments <- function(x, ...) x$response$content

#' @export
#' @keywords internal
as.data.frame.Tags <- function(x, ...) x$response$content

#' @export
#' @keywords internal
as.data.frame.Users <- function(x, ...) x$response$content

#' @export
#' @keywords internal
as.data.frame.Metrics <- function(x, ...) x$response

#' @export
#' @keywords internal
as.data.frame.DateRanges <- function(x, ...) x$response$content

##### as.data.frame methods for S3 classes that already return df

#' @export
#' @keywords internal
as.data.frame.CalculatedMetricsList <- function(x, ...) dplyr::bind_rows(lapply(x, as.data.frame))

#' @export
#' @keywords internal
as.data.frame.DateRangesList <- function(x, ...) dplyr::bind_rows(lapply(x, as.data.frame))

#' @export
#' @keywords internal
as.data.frame.ReportSuitesList <- function(x, ...) dplyr::bind_rows(lapply(x, as.data.frame))

#' @export
#' @keywords internal
as.data.frame.SegmentsList <- function(x, ...) dplyr::bind_rows(lapply(x, as.data.frame))

#' @export
#' @keywords internal
as.data.frame.UsersList <- function(x, ...) dplyr::bind_rows(lapply(x, as.data.frame))

##### as.data.frame methods for S3 classes that need custom df logic

#' @export
#' @keywords internal
as.data.frame.CompanyAccess <- function(x, ...) {
  temp <- x[[2]]

  df <- cbind(
    temp$imsUserId,
    temp$imsOrgs$imsOrgId,
    temp$imsOrgs$companies[[1]],
    stringsAsFactors = FALSE
  )

  names(df) <- c("imsUserId",
                 "imsOrgId",
                 "globalCompanyId",
                 "companyName",
                 "apiRateLimitPolicy")

  return(df)

}

