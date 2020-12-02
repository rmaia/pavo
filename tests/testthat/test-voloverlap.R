data(sicalis)
tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")

test_that("Overlap", {

  expect_equal(sum(voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex")), 0.1972758, tolerance = 1e-5)
  expect_equal(sum(voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex")), 9.922872e-06, tolerance = 1e-7)
  expect_equal(sum(voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex")[1:2]), 1.146523e-05, tolerance = 1e-6)

})

test_that("tcs", {

  tcs_sicalis <- colspace(vismodel(sicalis))
  vol_sicalis <- voloverlap(tcs_sicalis, tcs_sicalis, type = "convex")

  expect_length(vol_sicalis, 5)
  expect_equal(vol_sicalis$vboth, 1)
  expect_identical(vol_sicalis$vol1, vol_sicalis$vol2)
})

test_that("tri", {

  tri_sicalis <- colspace(vismodel(sicalis, visual = "ctenophorus"))
  vol_sicalis <- voloverlap(tri_sicalis, tri_sicalis, type = "convex")

  expect_length(vol_sicalis, 5)
  expect_equal(vol_sicalis$overlapvol, 0.00288459, tolerance = 1e-6)
})

test_that("Dataframe", {
  hrep <- rbind(
    c(1, 1, 0),
    c(-1, 0, 0),
    c(0, -1, 0),
    c(0, 0, -1),
    c(-1, -1, -1)
  )
  qux <- rbind(
    c(2, 0, 0),
    c(3, 1, 0),
    c(4, 0, 1),
    c(-7, -1, -1)
  )

  #  expect_error(voloverlap(hrep, qux), "dimnames")

  colnames(hrep) <- c("x", "y", "z")
  colnames(qux) <- c("x", "y", "z")

  vol <- voloverlap(hrep, qux, type = "convex")

  expect_length(vol, 5)
  expect_equal(vol$vol1, 2.5 / 3)
  expect_equal(vol$vol2, 1)
})

test_that("Symmetric", {
  hrep <- rbind(
    c(1, 1, 0),
    c(-1, 0, 0),
    c(0, -1, 0),
    c(0, 0, -1),
    c(-1, -1, -1)
  )
  qux <- rbind(
    c(2, 0, 0),
    c(3, 1, 0),
    c(4, 0, 1),
    c(-7, -1, -1)
  )

  colnames(hrep) <- c("x", "y", "z")
  colnames(qux) <- c("x", "y", "z")

  vol_hq <- voloverlap(hrep, qux, type = "convex")
  vol_qh <- voloverlap(qux, hrep, type = "convex")

  expect_identical(vol_hq$overlapvol, vol_qh$overlapvol)
  expect_identical(vol_hq$vsmallest, vol_qh$vsmallest)
  expect_identical(vol_hq$vboth, vol_hq$vboth)
})

test_that("Plane", {
  data(sicalis)

  vm_sicalis <- vismodel(sicalis)
  tcs_sicalis <- colspace(vm_sicalis)

  plane_sicalis <- tcs_sicalis[1:3, ]

  expect_error(suppressWarnings(voloverlap(plane_sicalis, plane_sicalis, type = "convex"), "error code 1"))
})

test_that("Alphashapes", {

  skip_if_not_installed("alphashape3d")

  overlap_alpha <- expect_silent(
    voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "alpha", avalue = 0.5)
  )

  expect_equal(overlap_alpha[[1]], 5.183721e-06, tolerance = 1e-7)
  expect_equal(overlap_alpha[[2]], 6.231493e-06, tolerance = 1e-7)

})
