data(flowers)
data(sicalis)
data(teal)

test_that("coldist", {
  # JND transform
  vis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)
  result1 <- jndrot(jnd2xyz(coldist(vismodel(flowers, achromatic = "bt.dc", relative = FALSE), achromatic = TRUE)))

  expect_equal(dim(result1), c(36, 4))
  expect_equal(round(mean(unlist(result1)^3), 3), -132.915)

  # Output
  result2 <- coldist(colspace(vismodel(flowers, visual = "canis", achromatic = "ml")), achromatic = TRUE)
  expect_equal(dim(result2), c(630, 4))
  expect_equal(round(mean(unlist(result2[, 3:4])), 4), 0.8693)

  result3 <- coldist(colspace(vismodel(flowers, visual = "canis", achromatic = "all")),
    n = c(1, 2), achromatic = TRUE, subset = "Hibbertia_acicularis"
  )
  expect_equal(dim(result3), c(35, 4))
  expect_equal(round(mean(unlist(result3[, 3:4])), 4), 0.5388)

  result4 <- coldist(
    colspace(vismodel(flowers,
      visual = "apis", achromatic = "all",
      relative = FALSE, vonkries = TRUE
    ), space = "hexagon"),
    n = c(1, 2), achromatic = TRUE, subset = c("Hibbertia_acicularis", "Grevillea_buxifolia")
  )
  expect_equal(dim(result4), c(1, 4))
  expect_equal(round(mean(unlist(result4[, 3:4])), 4), 0.464)

  result5 <- coldist(colspace(vismodel(flowers, visual = "segment")), achromatic = TRUE)
  expect_equal(dim(result5), c(630, 4))
  expect_equal(round(mean(unlist(result5[, 3:4])), 4), 0.2685)

  result6 <- coldist(colspace(vismodel(flowers,
    visual = "cie10",
    illum = "D65",
    vonkries = TRUE,
    relative = FALSE
  ), "cielab"))
  expect_equal(dim(result6), c(630, 4))
  expect_equal(round(mean(unlist(result6[, 3])), 4), 28.7164)

  result7 <- coldist(as.matrix(vismodel(flowers, achro = "bt.dc")), qcatch = "Qi", achromatic = TRUE)
  expect_equal(dim(result7), c(630, 4))
  expect_equal(round(mean(unlist(result7[, 3])), 4), 11.7606)
})

test_that("bootcoldist", {
  # Empirical means
  data(sicalis)
  vm.sic <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr.sic <- gsub("ind..", "", rownames(vm.sic))
  bcd.sic <- suppressWarnings(bootcoldist(
    vm.sic,
    by = gr.sic,
    n = c(1, 2, 3),
    weber = 0.1,
    weber.achro = 0.1
  ))
  expect_equal(
    unname(round(c(bcd.sic[, 1], bcd.sic[, 4]), 4)),
    c(4.5760, 1.1340, 5.6378, 8.2510, 0.0124, 8.2387)
  )
})

test_that("special_colspace", {
  # Dispace
  result8 <- colspace(vismodel(flowers, visual = "canis", achromatic = "all"))
  expect_equal(dim(result8), c(36, 5))
  expect_equal(round(mean(unlist(result8)), 4), 0.4061)

  # Trispace
  result9 <- colspace(vismodel(flowers, visual = "apis", achromatic = "l"))
  expect_equal(dim(result9), c(36, 8))
  expect_equal(round(mean(unlist(result9)), 4), 0.0876)

  # tcs
  result10 <- colspace(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc"))
  expect_equal(dim(result10), c(36, 17))
  expect_equal(round(mean(unlist(result10)), 4), 0.0923)

  # categorical
  result11 <- colspace(vismodel(flowers, visual = "musca", achro = "md.r1"), space = "categorical")
  expect_equal(dim(result11), c(36, 10))
  expect_equal(round(mean(unlist(result11[, -7])), 4), -0.0949)

  # segment
  result12 <- colspace(vismodel(flowers, visual = "segment", achromatic = "bt.dc"), space = "segment")
  expect_equal(dim(result12), c(36, 9))
  expect_equal(round(mean(unlist(result12)), 4), 20.4668)

  # coc
  result13 <- colspace(vismodel(flowers,
    visual = "apis", relative = FALSE, qcatch = "Ei",
    vonkries = TRUE, achromatic = "l"
  ), space = "coc")
  expect_equal(dim(result13), c(36, 7))
  expect_equal(round(mean(unlist(result13)), 4), 0.5304)

  # hexagon
  result14 <- colspace(vismodel(flowers,
    visual = "apis", qcatch = "Ei",
    vonkries = TRUE, relative = FALSE, achromatic = "l"
  ), space = "hexagon")
  expect_equal(dim(result14), c(36, 10))
  expect_equal(round(mean(unlist(result14[-9])), 4), 20.3489)

  # ciezyx
  result15 <- colspace(vismodel(flowers, visual = "cie10"), space = "ciexyz")
  expect_equal(dim(result15), c(36, 6))
  expect_equal(round(mean(unlist(result15)), 4), 0.3596)

  # cielab
  result16 <- colspace(vismodel(flowers, visual = "cie10"), space = "cielab")
  expect_equal(dim(result16), c(36, 6))
  expect_equal(round(mean(unlist(result16)), 4), 9.5446)

  # cielch
  result17 <- colspace(vismodel(flowers, visual = "cie10"), space = "cielch")
  expect_equal(dim(result17), c(36, 8))
  expect_equal(round(mean(unlist(result17)), 4), 26.2162)
})


test_that("voloverlap()", {
  tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
  tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
  tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")

  expect_equal(round(mean(unlist(voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex"))), 4), 0.0395)
  expect_equal(round(mean(unlist(voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex"))[1:2]^-1), 4), 201959.93)
})

test_that("processing & general", {
  # Sensdata
  result18 <- sensdata(illum = "all", bkg = "all", trans = "all")
  expect_equal(dim(result18), c(401, 7))
  expect_equal(round(mean(unlist(result18)), 4), 72.8731)

  # Peakshape
  result19 <- peakshape(flowers, absolute.min = TRUE)
  expect_equal(dim(result19), c(36, 7))
  expect_equal(round(mean(unlist(result19[, 2:6]), na.rm = TRUE), 4), 287.7377)

  # Simulate
  # Ideal
  result20 <- summary(simulate_spec(ylim = c(0, 50)))
  expect_equal(dim(result20), c(1, 23))
  expect_equal(round(mean(unlist(result20)), 4), 928.3311)

  # Sigmoidd low-high
  result21 <- summary(simulate_spec(wl_inflect = 550))
  expect_equal(dim(result21), c(1, 23))
  expect_equal(round(mean(unlist(subset(result21, select = -c(S2))), na.rm = TRUE), 4), 1479.6564)

  # Sigmoid high-low
  result22 <- summary(simulate_spec(wl_inflect = 550, ylim = c(100, 0)))
  expect_equal(dim(result22), c(1, 23))
  expect_equal(round(mean(unlist(subset(result22, select = -c(S2, S9))), na.rm = TRUE), 4), 1885.9745)

  # Gaussian
  result23 <- summary(simulate_spec(wl_peak = 400))
  expect_equal(dim(result23), c(1, 23))
  expect_equal(round(mean(unlist(subset(result23, select = -c(S2, S9))), na.rm = TRUE), 4), 694.4126)

  # Merge
  teal1 <- teal[, c(1, 3:5)]
  teal2 <- teal[, c(1, 2, 6:12)]
  expect_equal(round(mean(unlist(merge(teal1, teal2, by = "wl"))), 4), 51.1874)

  # Subset
  vis.sicalis <- vismodel(sicalis)
  tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
  expect_equal(round(mean(unlist(subset(vis.sicalis, "C")), na.rm = TRUE), 4), 0.25)
  expect_equal(round(mean(unlist(subset(sicalis, "T", invert = TRUE)), na.rm = TRUE), 4), 40.1018)

  # Summary
  expect_equal(round(mean(unlist(summary(teal))), 4), 287.9638)
  expect_equal(round(mean(unlist(subset(summary(sicalis), select = -c(S2)))), 4), 313.7217)
})

test_that("images", {
  papilio <-
    getimg(system.file("testdata", "images", "butterflies", "papilio.png", package = "pavo"))
  snakes <-
    getimg(system.file("testdata", "images", "snakes", package = "pavo"))

  expect_equal(unlist(unname(summary(papilio))), "papilio")
  expect_equal(unlist(unname(summary(snakes))), c("snake_01", "snake_02"))
})

test_that("vismodel", {
  result24 <- vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")
  expect_equal(dim(result24), c(36, 3))
  expect_equal(round(mean(unlist(result24)), 4), 0.4572)

  result25 <- vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)
  expect_equal(dim(result25), c(36, 4))
  expect_equal(round(mean(unlist(result25)), 4), 2.3597)

  result26 <- vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")
  expect_equal(dim(result26), c(36, 5))
  expect_equal(round(mean(unlist(result26)), 4), 0.2832)

  result27 <- vismodel(flowers, visual = "musca", achromatic = "md.r1", relative = FALSE)
  expect_equal(dim(result27), c(36, 5))
  expect_equal(round(mean(unlist(result27)), 4), 0.1904)

  result28 <- vismodel(flowers,
    visual = "apis", relative = FALSE, qcatch = "Ei", bkg = "green",
    vonkries = TRUE, achromatic = "l"
  )
  expect_equal(dim(result28), c(36, 4))
  expect_equal(round(mean(unlist(result28)), 4), 0.5457)

  result29 <- vismodel(flowers, visual = "cie10")
  expect_equal(dim(result29), c(36, 4))
  expect_equal(round(mean(unlist(result29), na.rm = TRUE), 4), 0.3859)
})
