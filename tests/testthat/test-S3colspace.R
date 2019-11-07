context("colspace")

# File to test S3 functions related to the colspace class

test_that("summary.colspace", {

  # Single spectrum for summary.colspace with tcspace
  data(sicalis)
  sicalis <- sicalis[, c(1,2)]

  vis_sicalis <- vismodel(sicalis)
  tcs_sicalis <- colspace(vis_sicalis)
  sumtcs_sicalis <- expect_warning(summary(tcs_sicalis),
                                   "Not enough points")
  expect_output(sumtcs_sicalis$huedisp.m, NA)
  expect_output(sumtcs_sicalis$huedisp.v, NA)
}
