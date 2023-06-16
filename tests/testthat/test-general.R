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

  test <- readRDS(file.path("known_output", "FWHM_lims.rds"))
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

test_that("simulate", {
  # Ideal reflectance
  spec_50 <- simulate_spec(ylim = c(0, 50))
  expect_identical(summary(spec_50)$B2, 50) # recover brightness
  expect_identical(summary(spec_50)$B3, 50) # recover brightness

  # Sigmoidal
  spec_sig1 <- simulate_spec(wl_inflect = 550) # low-high
  expect_identical(summary(spec_sig1)$H3, 550) # recover hue
  expect_identical(summary(spec_sig1)$S6, 100) # recover sat
  expect_identical(summary(spec_sig1)$B3, 100) # recover brightness

  spec_sig2 <- simulate_spec(wl_inflect = 550, ylim = c(100, 0)) # high-low
  expect_identical(summary(spec_sig2)$H3, 550) # recover hue
  expect_identical(summary(spec_sig2)$S6, 100) # recover sat
  expect_identical(summary(spec_sig2)$B3, 100) # recover brightness

  # Gaussian
  spec_gauss1 <- simulate_spec(wl_peak = 400)
  expect_identical(summary(spec_gauss1)$H1, 400) # recover hue
  expect_identical(summary(spec_gauss1)$S6, 100) # recover sat
  expect_identical(summary(spec_gauss1)$B3, 100) # recover brightness
})
