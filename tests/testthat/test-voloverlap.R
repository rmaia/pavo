library(pavo)
context("voloverlap")

test_that("tcs", {
  data(sicalis)

  tcs_sicalis <- colspace(vismodel(sicalis))
  vol_sicalis <- voloverlap(tcs_sicalis, tcs_sicalis)

  expect_length(vol_sicalis, 5)
  expect_equal(vol_sicalis$vboth, 1)
  expect_equal(vol_sicalis$vol1, vol_sicalis$vol2)
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

  expect_error(voloverlap(hrep, qux), "dimnames")

  colnames(hrep) <- c("x", "y", "z")
  colnames(qux) <- c("x", "y", "z")

  vol <- voloverlap(hrep, qux)

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

  vol_hq <- voloverlap(hrep, qux)
  vol_qh <- voloverlap(qux, hrep)

  expect_equal(vol_hq$overlapvol, vol_qh$overlapvol)
  expect_equal(vol_hq$vsmallest, vol_qh$vsmallest)
  expect_equal(vol_hq$vboth, vol_hq$vboth)
})

test_that("Plane", {
  data(sicalis)

  vm_sicalis <- vismodel(sicalis)
  tcs_sicalis <- colspace(vm_sicalis)

  plane_sicalis <- tcs_sicalis[1:3, ]

  expect_error(suppressWarnings(voloverlap(plane_sicalis, plane_sicalis), "error code 1"))
})
