context("vismodel")

# File to test S3 functions related to the vismodel class

test_that("summary.vismodel", {

  data(sicalis)
  vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
  summary_vis <- expect_output(summary(vis.sicalis), "visual model options")
  expect_is(summary_vis, "table")

  summary_subset <- expect_message(summary(vis.sicalis[, -5]), "subset data")
  expect_equivalent(summary_subset, summary.data.frame(vis.sicalis[, -5]))

})
