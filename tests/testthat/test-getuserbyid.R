test_that("Get User By ID", {

  skip_on_cran()

  gubi <- GetUserById()

  #Validate returned value is a data.frame
  expect_true("AnalyticsUser" %in% class(gubi))

})
