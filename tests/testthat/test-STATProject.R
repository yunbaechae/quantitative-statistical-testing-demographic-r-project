test_that("StatTest", {
  expect_s3_class(StatTest("ttest"),"myr")
  expect_type(StatTest("chitest"),"list")
})
