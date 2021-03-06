% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GetReportSuites.R
\name{GetReportSuites}
\alias{GetReportSuites}
\title{Get report suites for a company account}
\usage{
GetReportSuites(
  rsid = NULL,
  as.data.frame = TRUE,
  rsids = NULL,
  rsidContains = NULL,
  expansion = NULL
)
}
\arguments{
\item{as.data.frame}{(logical) Return result as data.frame}

\item{rsids}{(character) Only include suites in this RSID list
(comma-delimited)}

\item{rsidContains}{(character) Only include suites whose rsid contains
rsidContains}

\item{expansion}{(character) Comma-delimited list of additional metadata
fields to include on response}
}
\value{
data.frame or S3 'ReportSuites'
}
\description{
Get report suites for a company account
}
\details{
The 'expansion' keyword allows for specifying a list of additional
  fields to return as part of the response. These fields should be specified
  as a character vector (string), separated with commas and no spaces between
  the fields.

  Because of the complex nature of having unknown combinations of extra
  fields returned on a function call, using the 'expansion' field with
  as.data.frame=TRUE can return undesirable results.
}
\examples{
\dontrun{
grs <- GetReportSuites()
grs.nodf <- GetReportSuites(as.data.frame = FALSE)

}
}
