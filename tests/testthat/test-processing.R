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
  
  expect_message(as.rspec(fakedat), "wavelengths found")

  expect_warning(as.rspec(fakedat[, -1], lim = c(300, 700)), "user-specified range")
  expect_warning(as.rspec(fakedat[, -1]), "arbitrary index")

  expect_error(as.rspec(fakedat, lim = c(300.1, 700), interp = FALSE), "limits")
  expect_error(as.rspec(fakedat, lim = c(300, 699.1), interp = FALSE), "limits")
  
  fakedat[2:4, 2] <- NA
  expect_message(as.rspec(fakedat, whichwl = 1), 'negative')
  expect_message(as.rspec(fakedat, whichwl = 1), 'NA')
})

test_that("Procspec", {
  data(sicalis)

  # Errors
  expect_error(procspec(sicalis), "options selected")
  expect_error(procspec(sicalis, opt = "none", fixneg = "none"), "options selected")
  expect_error(procspec(sicalis, opt = "smooth", span = 0), "span")

  # Smoothing
  expect_equal(dim(procspec(sicalis, opt = "smooth")), dim(sicalis))
  expect_message(dim(procspec(sicalis, opt = "smooth")), "smoothing")
  expect_equal(
    dim(procspec(sicalis, opt = "smooth", span = 0.1)),
    dim(procspec(sicalis, opt = "smooth", span = 30)),
    dim(procspec(sicalis, opt = "smooth", span = 50))
  )

  # Binning
  expect_equal(dim(procspec(sicalis, opt = "bin", bins = 24)), c(24, 22))
  expect_equal(dim(procspec(sicalis, opt = "bin", bins = 33)), c(33, 22))

  # Minmax
  expect_equal(max(procspec(sicalis, opt = "maximum")[, -1]), 1)
  expect_equal(min(procspec(sicalis, opt = "minimum")[, -1]), 0)
  expect_equal(range(procspec(sicalis, opt = c("minimum", "maximum"))), c(0, 700))

  # Summing
  expect_equal(sum(apply(procspec(sicalis, opt = "sum")[, -1], 2, sum)), ncol(sicalis[, -1]))

  # Centering
  expect_equal(dim(procspec(sicalis, opt = "center")), dim(sicalis))

  # Fixing negs
  sicalis2 <- sicalis
  sicalis2[, 2:4] <- sicalis[, 2:4] * -1
  expect_false(any(procspec(sicalis2, fixneg = "zero") < 0))
  expect_false(any(procspec(sicalis2, fixneg = "addmin") < 0))

  # Everything
  expect_equal(
    dim(procspec(sicalis,
      opt = c("minimum", "maximum", "bin", "center", "sum"),
      span = 0.5,
      bins = 24
    )),
    c(24, 22)
  )
})

test_that("Aggregation", {
  library(digest)
  data(teal)
  
  ind <- rep(c('a', 'b'), times = 6)
  expect_equal(dim(aggspec(teal, by = ind)), c(401, 3))
  expect_equal(dim(aggspec(teal, by = 6)), c(401, 3))
  expect_equal(dim(aggspec(teal[,-1], by = ind)), c(401, 3))
  expect_equal(dim(aggspec(teal)), c(401, 2))
  
  teal1 <- teal[, c(1, 3:5)]
  teal2 <- teal[, c(1, 2, 6:12)]
  expect_equal(digest::sha1(merge(teal1, teal2, by = 'wl')), "3e6f9764b7b567176e9be4a0f616c59075a3b03b")
  
  data(sicalis)
  vis.sicalis <- vismodel(sicalis)
  tcs.sicalis <- colspace(vis.sicalis, space = 'tcs')

  # Subset all 'crown' patches (C in file names)
  expect_equal(digest::sha1(subset(vis.sicalis, "C")),  "df58525a36ec7df9cce6d234e8fa4ad55d67b481")
  expect_equal(digest::sha1(subset(sicalis, "T", invert = TRUE)), "3666588f654cf988cd2b12f39f00bbee7aaeae4a") 
  
})
