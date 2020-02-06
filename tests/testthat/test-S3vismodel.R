context("vismodel")

# File to test S3 functions related to the vismodel class

test_that("summary.vismodel", {

  data(sicalis)
  vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
  summary_vis <- expect_output(summary(vis.sicalis), "visual model options")
  expect_is(summary_vis, "table")

})
