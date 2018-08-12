test_that("Get User Company Access", {

  skip_on_cran()

  guca <- GetUserCompanyAccess()

  #Validate returned value is a data.frame
  expect_that(class(guca), "data.frame")

})
