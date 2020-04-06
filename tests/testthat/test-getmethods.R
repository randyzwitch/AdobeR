test_that("Test Get Methods", {

  skip_on_cran()

  AdobeJWT(privatekey = Sys.getenv("privatekey", ""),
           orgid = Sys.getenv("orgid", ""),
           techacctid = Sys.getenv("techacctid", ""),
           clientid = Sys.getenv("clientid", ""),
           secret = Sys.getenv("secret", ""))

# GetUserCompanyAccess ----------------------------------------------------

  # Default kwarg is for returning a data.frame
  guca <- GetUserCompanyAccess()
  expect_is(guca, "data.frame")

  # Test that S3 method returned
  guca.nodf <- GetUserCompanyAccess(as.data.frame = FALSE)
  expect_is(guca.nodf, "CompanyAccess")


# GetUsers ----------------------------------------------------------------

  users <- GetUsers()
  expect_is(users, "data.frame")

  users.nodf <- GetUsers(as.data.frame = FALSE)
  expect_is(users.nodf, "UsersList")


# GetCalculatedMetrics ----------------------------------------------------

  cm <- GetCalculatedMetrics()
  expect_is(cm, "data.frame")

  cm.nodf <- GetCalculatedMetrics(as.data.frame=FALSE)
  expect_is(cm.nodf, "CalculatedMetricsList")


# GetSegments -------------------------------------------------------------

  seg <- GetSegments()
  expect_is(seg, "data.frame")

  seg.nodf <- GetSegments(as.data.frame = FALSE)
  expect_is(seg.nodf, "SegmentsList")


# GetReportSuites ---------------------------------------------------------

  grs <- GetReportSuites()
  expect_is(grs, "data.frame")

  grs.nodf <- GetReportSuites(as.data.frame = FALSE)
  expect_is(grs.nodf, "ReportSuitesList")


# GetDimensions -----------------------------------------------------------

  dims <- GetDimensions("zwitchdev")
  expect_is(dims, "data.frame")

  dims.nodf <- GetDimensions("zwitchdev", as.data.frame = FALSE)
  expect_is(dims.nodf, "Dimensions")


# GetMetrics --------------------------------------------------------------

  metrics <- GetMetrics("zwitchdev")
  expect_is(metrics, "data.frame")

  metrics.nodf <- GetMetrics("zwitchdev", as.data.frame = FALSE)
  expect_is(metrics.nodf, "Metrics")


# GetDateRanges -----------------------------------------------------------

  gdr <- GetDateRanges()
  expect_is(gdr, "data.frame")

  gdr.nodf <- GetDateRanges(as.data.frame = FALSE)
  expect_is(gdr.nodf, "DateRangesList")

})
