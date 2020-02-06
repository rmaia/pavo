context("colspace")

# File to test S3 functions related to the colspace class
data(sicalis)

test_that("summary.colspace", {

  # Single spectrum for summary.colspace with tcspace

  sicalis <- sicalis[, c(1, 2)]

  vis_sicalis <- vismodel(sicalis)
  tcs_sicalis <- colspace(vis_sicalis)
  sumtcs_sicalis <- expect_warning(
    summary(tcs_sicalis),
    "Not enough points"
  )
  expect_output(sumtcs_sicalis$huedisp.m, NA)
  expect_output(sumtcs_sicalis$huedisp.v, NA)
})

test_that("subset.colspace", {

  tcs_sicalis <- colspace(vismodel(sicalis))

  tcs_sicalis_T <- subset(tcs_sicalis, "T")

  expect_equal(dim(tcs_sicalis_T), c(7, 17))
  expect_is(tcs_sicalis_T, "colspace")

})
