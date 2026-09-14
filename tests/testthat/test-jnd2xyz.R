data(flowers)

test_that("JND space for dichromat", {

  canis.flowers <- vismodel(flowers, visual = "canis")
  cd.flowers <- coldist(canis.flowers, n = c(1, 1))

  jnd_x <- jnd2xyz(cd.flowers, rotate = FALSE)

  jnd_x_rot <- jnd2xyz(cd.flowers, rotate = TRUE)

  expect_snapshot(jnd_x_rot)

  # Rotation doesn't change the distances
  expect_equal(
    dist(jnd_x),
    dist(jnd_x_rot),
    ignore_attr = "call"
  )

})

test_that("JND space for trichromat", {

  apis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(apis.flowers, n = c(1, 1, 1))

  jnd_xy <- jnd2xyz(cd.flowers, rotate = FALSE)

  jnd_xy_rot <- jnd2xyz(cd.flowers, rotate = TRUE)

  expect_snapshot(jnd_xy_rot)

  # Rotation doesn't change the distances
  expect_equal(
    dist(jnd_xy),
    dist(jnd_xy_rot),
    ignore_attr = "call"
  )

})

test_that("JND space for tetrachromat", {

  bluetit.flowers <- vismodel(flowers, visual = "bluetit")
  cd.flowers <- coldist(bluetit.flowers)

  jnd_xyz <- jnd2xyz(cd.flowers, rotate = FALSE)

  jnd_xyz_rot <- jnd2xyz(cd.flowers, rotate = TRUE)

  expect_snapshot(jnd_xyz_rot)

  # Rotation doesn't change the distances
  expect_equal(
    dist(jnd_xyz),
    dist(jnd_xyz_rot),
    ignore_attr = "call"
  )
})

test_that("JND space places the achromatic reference under quantum noise", {
  # coldist() returns NA reference achromatic distances under quantum noise, and
  # coldist2mat() then replaces them with zeros, so the achromatic reference sits
  # at no distance from any sample. The failure is silent: no NA survives to be
  # noticed, and with center = TRUE the centroid sweep removes the bad offset from
  # the sample coordinates entirely, leaving it only in the stored references.
  bluetit.flowers <- vismodel(flowers,
    visual = "bluetit", achromatic = "bt.dc",
    relative = FALSE, scale = 10000
  )
  cd.flowers <- suppressMessages(
    coldist(bluetit.flowers, noise = "quantum", achromatic = TRUE)
  )

  # Luminance coordinates are always measured from the achromatic reference, so
  # with center = FALSE the first sample sits exactly its own achromatic distance
  # from a notionally black stimulus of 1e-10 in every channel
  jnd_xyz <- jnd2xyz(cd.flowers, rotate = FALSE, center = FALSE)

  qlum <- bluetit.flowers[["lum"]][1]
  expected <- abs(log(qlum) - log(1e-10)) /
    sqrt(0.1^2 + 2 / (qlum + 1e-10))

  expect_true("lum" %in% names(jnd_xyz))
  expect_equal(
    abs(jnd_xyz[rownames(bluetit.flowers)[1], "lum"]),
    expected,
    tolerance = 1e-6
  )
})
