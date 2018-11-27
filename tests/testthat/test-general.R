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

test_that("plot utilities", {
  data(sicalis)
  # expect_known_hash(spec2rgb(sicalis), "0d3e41a7b6")

  expect_error(spec2rgb(sicalis[300:nrow(sicalis), ]), "full visible range")
  expect_error(spec2rgb(sicalis[, -1]), "No wavelengths supplied")
})
