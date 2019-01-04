context("plots")
library(vdiffr)

test_that("rspec", {
  skip_on_cran()

  data(sicalis)

  expect_doppelganger("plot.rspec-overlay", plot(sicalis, col = spec2rgb(sicalis)))
  expect_identical({plot(sicalis, type = "overlay")}, {plot(sicalis)})
  expect_doppelganger("plot.rspec-stack", plot(sicalis, type = "stack"))
  expect_doppelganger("explorespec", {par(mfrow = c(3,3)); explorespec(sicalis[,1:10])})
  expect_doppelganger("aggplot", aggplot(sicalis, by = 3))
  expect_doppelganger("aggplot-quantile", aggplot(sicalis, by = 3, FUN.error = function(x) quantile(x, c(0.0275, 0.975))) )
  expect_doppelganger("aggplot-shadecol", aggplot(sicalis, by = 3, shadecol = spec2rgb(sicalis), lcol = 1))
  expect_doppelganger("plotsmooth", plotsmooth(sicalis[1:7], ask = FALSE))
  expect_doppelganger("peakshape", {par(mfrow = c(3,3)); peakshape(sicalis[,1:10])})

  data(teal)
  expect_doppelganger("plot.rspec-heatmap", plot(sicalis, type = "heatmap"))
})

test_that("colspace", {
  skip_on_cran()

  data(sicalis)

  tcs_sicalis <- colspace(vismodel(sicalis, relative = TRUE))

  expect_doppelganger("tetraplot", plot(tcs_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(tcs_sicalis), tetraplot(tcs_sicalis))

  tri_sicalis <- colspace(vismodel(sicalis,, visual = "apis"))

  expect_doppelganger("triplot", plot(tri_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(tri_sicalis), triplot(tri_sicalis))

  di_sicalis <- colspace(vismodel(sicalis, visual = "canis"))

  expect_doppelganger("diplot", plot(di_sicalis, col = spec2rgb(sicalis)))
  expect_identical(plot(di_sicalis), diplot(di_sicalis))

  ciexyz_sicalis <- colspace(vismodel(sicalis, visual = "cie10"))
  cielch_sicalis <- colspace(vismodel(sicalis, visual = "cie10"), space = "cielch")
  cielab_sicalis <- colspace(vismodel(sicalis, visual = "cie10"), space = "cielab")

  expect_doppelganger("ciexyz", plot(ciexyz_sicalis))
  expect_doppelganger("cielch", plot(cielch_sicalis))
  expect_doppelganger("cielab", plot(cielab_sicalis))

  for (x in c(ciexyz_sicalis, cielch_sicalis, cielab_sicalis)) {
    expect_identical(plot(x), cieplot(x))
  }

})

