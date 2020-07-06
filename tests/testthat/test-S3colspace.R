context("colspace")

# File to test S3 functions related to the colspace class
data(sicalis)

test_that("summary.colspace", {

  # Single spectrum for summary.colspace with tcspace

  sicalis <- sicalis[, c(1, 2)]

  vis_sicalis <- vismodel(sicalis)
  tcs_sicalis <- colspace(vis_sicalis)
  sumtcs_sicalis <- expect_warning(
    expect_output(summary(tcs_sicalis), "Colorspace & visual"),
    "Not enough points"
  )
  expect_true(is.na(sumtcs_sicalis$huedisp.m))
  expect_true(is.na(sumtcs_sicalis$huedisp.v))

  summary_subset <- expect_message(summary(tcs_sicalis[, -17]), "subset data")
  expect_equal(summary_subset, summary.data.frame(tcs_sicalis[, -17]))

  tri_sicalis <- colspace(vismodel(sicalis, "cie10"), "tri")
  expect_equal(
    expect_output(summary(tri_sicalis)),
    summary.data.frame(tri_sicalis)
  )

  data(flowers)

  expect_equal(sum(summary(colspace(vismodel(flowers)))), 4.0958993)

})

test_that("subset.colspace", {

  tcs_sicalis <- colspace(vismodel(sicalis))

  tcs_sicalis_T <- subset(tcs_sicalis, "T")

  expect_equal(dim(tcs_sicalis_T), c(7, 17))
  expect_s3_class(tcs_sicalis_T, "colspace")

})
