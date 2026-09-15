test_that("Procspec", {
  data(sicalis)

  # Errors
  expect_error(procspec(sicalis), "options selected")
  expect_error(procspec(sicalis, opt = "none", fixneg = "none"), "options selected")
  expect_error(procspec(sicalis, opt = "smooth", span = 0), "span")

  # Clipping
  expect_identical(dim(procspec(sicalis, opt = "clip", clip_range = c(400, 500))), dim(sicalis))
  expect_message(dim(procspec(sicalis, opt = "clip", clip_range = c(400, 500))), "clipping")
  expect_identical(
    dim(procspec(sicalis, opt = "clip", clip_range = list(c(400, 500), c(550, 650)))),
    dim(sicalis)
  )
  expect_error(procspec(sicalis, opt = "clip"), "clip_range")
  expect_error(procspec(sicalis, opt = "clip", clip_range = "hello"), "clip_range")
  expect_error(procspec(sicalis, opt = "clip", clip_range = c(400, 500, 600)), "clip_range")
  expect_error(procspec(sicalis, opt = "clip", clip_range = c(500, 400)), "clip_range")
  clipped <- procspec(sicalis, opt = "clip", clip_range = c(400, 500))
  rfrom <- clipped[clipped$wl == 400, -1]
  rto <- clipped[clipped$wl == 500, -1]
  rmid <- clipped[clipped$wl == 450, -1]
  expect_equal(rmid, (rto + rfrom) / 2, tolerance = 1e-06, ignore_attr = c("class", "row.names"))
  clipped <- procspec(sicalis, opt = "clip", clip_range = list(c(400, 500), c(550, 650)))
  rfrom <- clipped[clipped$wl == 550, -1]
  rto <- clipped[clipped$wl == 650, -1]
  rmid <- clipped[clipped$wl == 600, -1]
  expect_equal(rmid, (rto + rfrom) / 2, tolerance = 1e-06, ignore_attr = c("class", "row.names"))

  # Smoothing
  expect_identical(dim(procspec(sicalis, opt = "smooth")), dim(sicalis))
  expect_message(dim(procspec(sicalis, opt = "smooth")), "smoothing")
  expect_identical(
    dim(procspec(sicalis, opt = "smooth", span = 0.1)),
    dim(procspec(sicalis, opt = "smooth", span = 30))
  )
  expect_identical(
    dim(procspec(sicalis, opt = "smooth", span = 0.1)),
    dim(procspec(sicalis, opt = "smooth", span = 50))
  )

  # Binning
  expect_identical(dim(procspec(sicalis, opt = "bin", bins = 24)), c(24L, 22L))
  expect_identical(dim(procspec(sicalis, opt = "bin", bins = 33)), c(33L, 22L))

  # Minmax. Per spectrum, not globally: a global range() is satisfied as soon as
  # one spectrum reaches each bound, and sicalis happens to contain two spectra
  # whose minimum is already zero.
  nspec <- ncol(sicalis) - 1L
  maxima <- function(x) unname(apply(x[, -1], 2, max))
  minima <- function(x) unname(apply(x[, -1], 2, min))

  expect_identical(maxima(procspec(sicalis, opt = "maximum")), rep(1, nspec))
  expect_identical(minima(procspec(sicalis, opt = "minimum")), rep(0, nspec))

  scaled <- procspec(sicalis, opt = c("minimum", "maximum"))
  expect_identical(minima(scaled), rep(0, nspec))
  expect_identical(maxima(scaled), rep(1, nspec))

  # Summing
  expect_equal(
    sum(colSums(procspec(sicalis, opt = "sum")[, -1])),
    ncol(sicalis[, -1]),
    tolerance = 1e-14
  )

  # Centering
  expect_identical(dim(procspec(sicalis, opt = "center")), dim(sicalis))

  # Fixing negs
  sicalis2 <- sicalis
  sicalis2[, 2:4] <- sicalis[, 2:4] * -1
  expect_false(any(procspec(sicalis2, fixneg = "zero") < 0))
  expect_false(any(procspec(sicalis2, fixneg = "addmin") < 0))

  # Everything
  expect_identical(
    dim(procspec(sicalis,
      opt = c("minimum", "maximum", "bin", "center", "sum"),
      span = 0.5,
      bins = 24
    )),
    c(24L, 22L)
  )

  # Uninterpolated spectra
  uninterp <- lightr::lr_get_spec(
    system.file("testdata", "heliomaster", package = "lightr"),
    ext = "jdx", interpolate = FALSE
  )
  uninterp_sm <- procspec(uninterp, "smooth", span = 0.1)

  vdiffr::expect_doppelganger(
    "uninterpolated_procspec",
    plot(uninterp_sm)
  )
})

test_that("procspec() scales from the current values, not the input's", {
  # a is all positive, b runs negative, and both extremes are known exactly.
  tiny <- data.frame(wl = 300:305, a = c(2, 4, 6, 8, 10, 12), b = c(-1, 0, 1, 2, 3, 4))
  class(tiny) <- c("rspec", "data.frame")
  ramp <- c(0, 0.2, 0.4, 0.6, 0.8, 1)

  # Subtracting the minimum changes the maximum, so dividing by the input's
  # maximum leaves a short of 1 and carries b past it.
  scaled <- procspec(tiny, opt = c("minimum", "maximum"))
  expect_equal(scaled$a, ramp)
  expect_equal(scaled$b, ramp)

  # fixneg clamps b's negative value away, so the minimum left to subtract is
  # the clamped 0, not the original -1.
  zeroed <- procspec(tiny, opt = "minimum", fixneg = "zero")
  expect_equal(zeroed$b, c(0, 0, 1, 2, 3, 4))
  expect_equal(zeroed$a, c(0, 2, 4, 6, 8, 10))

  # addmin raises b by 1, taking its maximum to 5.
  shifted <- procspec(tiny, opt = "maximum", fixneg = "addmin")
  expect_equal(shifted$b, ramp)
  expect_equal(shifted$a, (1:6) / 6)
})

test_that("Aggregation", {
  data(teal)

  ind <- rep(c("a", "b"), times = 6)
  expect_identical(dim(aggspec(teal, by = ind)), c(401L, 3L))
  expect_identical(dim(aggspec(teal, by = 6)), c(401L, 3L))
  expect_identical(dim(aggspec(teal[, -1], by = ind)), c(401L, 3L))
  expect_identical(dim(aggspec(teal)), c(401L, 2L))

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
  expect_identical(spec2rgb(teal)[1], c("Acrecca-01" = "#21B662FF"))
})
