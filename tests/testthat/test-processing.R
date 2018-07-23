library(pavo)
context("processing")

test_that("Conversion to rspec", {
  fakedat <- data.frame(wl = seq(200, 800, 0.5), refl1 = rnorm(1201), refl2 = rnorm(1201))

  expect_equal(dim(as.rspec(fakedat, lim = c(300, 700), interp = FALSE)), c(801, 3))
  expect_equal(dim(as.rspec(fakedat, lim = c(200, 800), interp = FALSE)), c(1201, 3))
  expect_equal(dim(as.rspec(fakedat, lim = c(300, 700), interp = TRUE)), c(401, 3))
  expect_equal(as.data.frame(as.rspec(fakedat, lim = c(300, 700))["wl"]), data.frame(wl = as.numeric(c(300:700))))

  # Matrix and df input should have the same output
  expect_equal(as.rspec(fakedat), as.rspec(as.matrix(fakedat)))

  # Single column df should work and output a 2 columns rspec object
  expect_equal(dim(suppressWarnings(as.rspec(fakedat[, 2, drop = FALSE]))), c(1201, 2))

  expect_silent(as.rspec(fakedat, whichwl = 1))

  expect_message(as.rspec(fakedat), "wavelengths found")

  expect_warning(as.rspec(fakedat[, -1], lim = c(300, 700)), "user-specified range")
  expect_warning(as.rspec(fakedat[, -1]), "arbitrary index")

  expect_error(as.rspec(fakedat, lim = c(300.1, 700), interp = FALSE), "limits")
  expect_error(as.rspec(fakedat, lim = c(300, 699.1), interp = FALSE), "limits")
})

test_that("Spectra pre-processing", {
  data(sicalis)

  expect_equal(dim(procspec(sicalis, opt = "bin", bins = 24)), c(24, 22))
  expect_equal(dim(procspec(sicalis, opt = "bin", bins = 33)), c(33, 22))
})
