library(pavo)
context("general")

test_that("Class assignment", {
  data(flowers)

  # Check rspec
  # See test-S3rspec.R

  # Check vismodel
  vis.flowers <- vismodel(flowers, visual = "apis")
  expect_is(vis.flowers, "vismodel")

  # Check a few colorspaces
  vis.cie <- vismodel(flowers, vonkries = TRUE, relative = FALSE, achro = "none", visual = "cie10")
  col.cie <- colspace(vis.cie, space = "cielab")
  expect_is(col.cie, "colspace")

  vis.tcs <- vismodel(flowers, visual = "bluetit")
  col.tcs <- colspace(vis.tcs, space = "tcs")
  expect_is(col.cie, "colspace")

  vis.hex <- vismodel(flowers, relative = FALSE, qcatch = "Ei", vonkries = TRUE, visual = "apis")
  col.hex <- colspace(vis.hex, space = "hex")
  expect_is(col.hex, "colspace")
})

test_that("sensdata", {
  expect_true(all(names(as.data.frame(vissyst)) %in% names(sensdata('all', 'all'))))
})

test_that("peakshape", {
  data(flowers)

  expect_equivalent(round(colSums(peakshape(flowers, select = 1:5, lim = c(300, 700), plot = FALSE)[2:3])), c(916, 3317))

  test <- read.csv(system.file("testdata/FWHM_lims.csv", package = "pavo"))
  expect_equal(peakshape(test, plot = FALSE)[,4], c(144, 52))
})
