test_that("Class assignment", {
  data(flowers)

  # Check a few colorspaces
  vis.cie <- vismodel(flowers, vonkries = TRUE, relative = FALSE, achro = "none", visual = "cie10")
  col.cie <- colspace(vis.cie, space = "cielab")
  expect_s3_class(col.cie, "colspace")

  vis.tcs <- vismodel(flowers, visual = "bluetit")
  col.tcs <- colspace(vis.tcs, space = "tcs")
  expect_s3_class(col.cie, "colspace")

  vis.hex <- vismodel(flowers, relative = FALSE, qcatch = "Ei", vonkries = TRUE, visual = "apis")
  col.hex <- colspace(vis.hex, space = "hex")
  expect_s3_class(col.hex, "colspace")
})

test_that("sensdata", {
  expect_true(all(names(as.data.frame(vissyst)) %in% names(sensdata("all", "all"))))

  # Check for negative values
  expect_identical(min(sensdata(visual = "all", illum = "all", trans = "all", achromatic = "all", bkg = "all")), 0)
})

test_that("peakshape", {
  data(flowers)

  expect_equal(
    round(colSums(peakshape(flowers, select = 1:5, lim = c(300, 700), plot = FALSE)[2:3])),
    c(216, 2617),
    ignore_attr = TRUE
  )

  test <- readRDS("known_output/FWHM_lims.rds")
  expect_identical(peakshape(test, plot = FALSE)[, 4], c(144L, 52L))

  expect_warning(peakshape(flowers[, -1], plot = FALSE), "wl column missing")

  expect_identical(
    nrow(peakshape(flowers, startsWith(colnames(flowers), "Hibbertia"), plot = FALSE)),
    6L
  )

  # Double peak
  dblpkspec <- data.frame(
    wl = 300:700,
    spec = dnorm(300:700, 400, 10) + dnorm(300:700, 600, 10)
  )
  expect_warning(peakshape(dblpkspec), "Using first peak found")

  expect_null(peakshape(flowers, select = FALSE))

  expect_warning(
    peakshape(flowers, lim = c(300, 400), plot = FALSE),
    "incorporate all minima in spectral curves"
  )
})
