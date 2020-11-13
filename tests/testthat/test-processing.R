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
  data(teal)

  ind <- rep(c("a", "b"), times = 6)
  expect_equal(dim(aggspec(teal, by = ind)), c(401, 3))
  expect_equal(dim(aggspec(teal, by = 6)), c(401, 3))
  expect_equal(dim(aggspec(teal[, -1], by = ind)), c(401, 3))
  expect_equal(dim(aggspec(teal)), c(401, 2))

  teal1 <- teal[, c(1, 3:5)]
  teal2 <- teal[, c(1, 2, 6:12)]

  data(sicalis)
  vis.sicalis <- vismodel(sicalis)
  tcs.sicalis <- colspace(vis.sicalis, space = "tcs")

  expect_error(aggspec(teal, by = 7), "by not a multiple")
})

test_that("Convert", {
  # Flux/irrad
  illum <- sensdata(illum = "forestshade")
  expect_equal(sum(irrad2flux(illum)[2]), 6.619, tolerance = 10e-4)
  expect_equal(sum(flux2irrad(illum)[2]), 3174.328, tolerance = 10e-4)

  # RGB
  data(teal)
  expect_identical(spec2rgb(teal)[1], c("Acrecca-01" = "#1EB860FF"))
})
