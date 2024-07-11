data(flowers)

test_that("JND space for dichromat", {

  canis.flowers <- vismodel(flowers, visual = "canis")
  cd.flowers <- coldist(canis.flowers, n = c(1, 1))

  jnd_x <- jnd2xyz(cd.flowers)

  expect_snapshot(jnd_x)

  # After conversion to coordinates, the distance should not be modified
  expect_equal(
    as.matrix(dist(jnd_x, diag = TRUE, upper = TRUE)),
    coldist2mat(cd.flowers)[["dS"]]
  )

  # Rotate has no effect in 2D
  expect_identical(
    jnd2xyz(cd.flowers),
    jnd2xyz(cd.flowers, rotate = FALSE)
  )

})

test_that("JND space for trichromat", {

  apis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(apis.flowers, n = c(1, 1, 1))

  jnd_xy <- jnd2xyz(cd.flowers)

  expect_snapshot(jnd_xy)

  # After conversion to coordinates, the distance should not be modified
  expect_equal(
    as.matrix(dist(jnd_xy, diag = TRUE, upper = TRUE)),
    coldist2mat(cd.flowers)[["dS"]]
  )

})

test_that("JND space for tetrachromat", {

  bluetit.flowers <- vismodel(flowers, visual = "bluetit")
  cd.flowers <- coldist(bluetit.flowers)

  jnd_xyz <- jnd2xyz(cd.flowers)

  expect_snapshot(jnd_xyz)

  # After conversion to coordinates, the distance should not be modified
  expect_equal(
    as.matrix(dist(jnd_xyz, diag = TRUE, upper = TRUE)),
    coldist2mat(cd.flowers)[["dS"]]
  )

})
