test_that("launchApp returns a valid app directory", {
  appDir <- system.file("app", package = "HIViz")
  expect_true(dir.exists(appDir))
})