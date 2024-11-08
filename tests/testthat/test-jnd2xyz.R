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
