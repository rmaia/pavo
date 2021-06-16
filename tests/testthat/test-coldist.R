test_that("Errors", {
  data(flowers)

  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3, 4, 5), achro = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3), achro = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), subset = c("Goodenia", "Xyris", "Eucalyptus"), achro = FALSE), "Too many")
  expect_error(coldist(as.matrix(vismodel(flowers)), noise = "quantum"), "quantum receptor noise model")
  expect_error(coldist(vismodel(flowers, qcatch = "Ei")), "not compatible with hyperbolically")
  expect_error(coldist(as.matrix(vismodel(flowers))), "quantum catches not defined")
  expect_error(coldist(vismodel(flowers), weber.ref = 5), "greater than the length of vector")
})

test_that("Messages & warnings", {
  data(flowers)

  expect_message(coldist(vismodel(flowers, relative = FALSE)), "noise-weighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "segment"))), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "musca"), space = "categorical")), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "cie2"), space = "cielab")), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = "cie10"), space = "cielch")), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers,
    visual = "apis", relative = FALSE,
    qcatch = "Ei", vonkries = TRUE
  ), space = "coc")), "Manhattan distances")
  expect_message(coldist(colspace(vismodel(flowers,
    visual = "apis", relative = FALSE,
    qcatch = "Ei", vonkries = TRUE
  ), space = "hexagon")), "unweighted Euclidean")
  expect_message(coldist(as.matrix(vismodel(flowers, achro = "bt.dc")), qcatch = "Qi", achromatic = TRUE), "last column ignored for chromatic contrast")
  expect_message(coldist(as.matrix(vismodel(flowers)), qcatch = "Qi"), "Number of cones assumed to be 4")

  expect_message(coldist(vismodel(flowers)), "Quantum catch are relative")
  expect_message(coldist(vismodel(flowers), achromatic = TRUE), "achromatic contrast not calculated")

  expect_error(coldist(vismodel(flowers, relative = FALSE), noise = "quantum"), "negative quantum-catch")
})

test_that("Equivalent", {
  data(flowers)

  expect_equal(
    coldist(vismodel(flowers, relative = FALSE), weber.ref = "longest", achro = FALSE),
    coldist(vismodel(flowers, relative = FALSE), weber.ref = 4, achro = FALSE)
  )

  expect_equal(
    coldist(vismodel(flowers, relative = FALSE), weber.ref = "longest", achro = FALSE),
    coldist(vismodel(flowers, relative = FALSE), weber.ref = 4, achro = FALSE)
  )

  expect_equal(
    coldist(vismodel(flowers, relative = TRUE), achromatic = FALSE),
    suppressWarnings(coldist(vismodel(flowers, relative = FALSE), achro = FALSE)),
    ignore_attr = TRUE
  )

  expect_equal(
    suppressWarnings(coldist(vismodel(flowers))),
    suppressWarnings(coldist(as.data.frame(vismodel(flowers)), qcatch = "Qi")),
    ignore_attr = TRUE
  )

  expect_equal(
  coldist(colspace(vismodel(flowers, visual = "cie10", vonkries = TRUE, relative = FALSE), 'cielab')),
  coldist(colspace(vismodel(flowers, visual = "cie10", vonkries = TRUE, relative = FALSE), 'cielch')),
  ignore_attr = TRUE
  )

})

test_that("Options", {
  data(sicalis)

  expect_length(coldist(vismodel(sicalis, achromatic = "bt.dc", qcatch = "fi", illum = 1000, relative = FALSE),
    noise = "quantum",
    achromatic = TRUE
  ), 4)
})

test_that("jnd transform", {
  data(flowers)

  vis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)

  # Errors
  expect_equal(dim(jnd2xyz(cd.flowers)), c(36, 2))
  expect_equal(dim(coldist(vismodel(flowers))), c(630, 4))

  rownames(attr(jnd.flowers, "resref"))[4] <- "nope"
  expect_error(jndrot(jnd.flowers), "does not match")

  class(jnd.flowers) <- "data.frame"
  expect_error(jndrot(jnd.flowers), "jnd2xyz")
})

test_that("Output", {
  # Maximum possible unweighted Euclidean distances
  di <- data.frame(s = c(0, 1), l = c(1, 0))
  expect_equal(coldist(colspace(di, space = "di"))[["dS"]], (1 / sqrt(2)) * 2)

  tri <- data.frame(s = c(0, 0), m = c(0, 1), l = c(1, 0))
  expect_equal(coldist(colspace(tri, space = "tri"))[["dS"]], (1 / sqrt(2)) * 2)

  tetra <- data.frame(u = c(0, 1), s = c(0, 0), m = c(0, 0), l = c(1, 0))
  expect_equal(coldist(colspace(tetra, space = "tcs"))[["dS"]], (sqrt(3) / (2 * sqrt(2))) * 2)
})

test_that("bootcoldist", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))

  bcd <- suppressWarnings(
    bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1)
  )
  expect_equal(dim(bcd), c(3, 6))

  # Raw size
  raw <- bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 30, raw = TRUE)
  expect_equal(nrow(raw), 30)
  raw2 <- bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 437, raw = TRUE)
  expect_equal(nrow(raw2), 437)
})
