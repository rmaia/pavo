context("coldist")

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
  expect_message(coldist(colspace(vismodel(flowers, visual = 'segment'))), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = 'musca'), space = 'categorical')), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = 'cie2'), space = 'cielab')), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = 'cie10'), space = 'cielch')), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = 'apis', relative = FALSE,
                                           qcatch = 'Ei', vonkries = TRUE), space = 'coc')), "Manhattan distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = 'apis', relative = FALSE,
                                           qcatch = 'Ei', vonkries = TRUE), space = 'hexagon')), "unweighted Euclidean")

  expect_warning(coldist(vismodel(flowers)), "Quantum catch are relative")
  expect_warning(coldist(vismodel(flowers), achromatic = TRUE), "achromatic contrast not calculated")
  expect_warning(coldist(as.matrix(vismodel(flowers)), qcatch = "Qi"), "number of cones not specified; assumed to be 4")
  expect_warning(coldist(as.matrix(vismodel(flowers, achro = "bt.dc")), qcatch = "Qi", achromatic = TRUE), "last column ignored for chromatic contrast")
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
    check.attributes = FALSE
  )

  expect_equal(
    suppressWarnings(coldist(vismodel(flowers))),
    suppressWarnings(coldist(as.data.frame(vismodel(flowers)), qcatch = "Qi")),
    check.attributes = FALSE
  )
})

test_that("Options", {
  data(sicalis)

  expect_length(coldist(vismodel(sicalis, achromatic = "bt.dc", qcatch = "fi", relative = FALSE),
    noise = "quantum",
    achro = TRUE
  ), 4)
})

test_that("jnd transform", {
  data(flowers)
  library(digest)

  vis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)

  expect_equal(digest::sha1(jndrot(jnd2xyz(coldist(vismodel(flowers, achromatic = "bt.dc", relative = FALSE), achromatic = TRUE))), digits = 4), 
               "07064d68561bad24d8f02c0413611b5ba49ec53a")

  # Errors
  expect_equal(dim(jnd2xyz(cd.flowers)), c(36, 2))
  expect_equal(dim(coldist(vismodel(flowers))), c(630, 4))

  rownames(attr(jnd.flowers, "resref"))[4] <- "nope"
  expect_error(jndrot(jnd.flowers), "does not match")

  class(jnd.flowers) <- "data.frame"
  expect_error(jndrot(jnd.flowers), "jnd2xyz")
})

test_that("Output", {
  library(digest)
  data(flowers)

  # Maximum possible unweighted Euclidean distances
  di <- data.frame(s = c(0,1), l = c(1,0))
  expect_equal(coldist(colspace(di, space = 'di'))['dS'], (1 / sqrt(2)) * 2, check.attributes = FALSE)

  tri <- data.frame(s = c(0,0), m = c(0,1), l = c(1,0))
  expect_equal(coldist(colspace(tri, space = 'tri'))['dS'], (1 / sqrt(2)) * 2, check.attributes = FALSE)

  tetra <- data.frame(u = c(0, 1), s = c(0, 0), m = c(0, 0), l = c(1, 0))
  expect_equal(coldist(colspace(tetra, space = 'tcs'))['dS'], (sqrt(3)/(2*sqrt(2)))*2, check.attributes = FALSE)

  # Regression
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = 'canis', achromatic = 'ml')), achromatic = TRUE), digits = 4), 
               "bc460149b2263a857c9d573e77169556fa641f56")
  expect_equal(digest::sha1(coldist(vismodel(flowers, visual = 'canis', achromatic = 'ml'), achromatic = TRUE, n = c(1, 1)), digits = 4), 
               "7329a3c550fe1d2939423e4104066c868891914f")
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = "canis", achromatic = "all")), n = c(1, 2), achromatic = TRUE, subset = "Hibbertia_acicularis"), digits = 4), 
               "27ab9af8efe2b1651cd36f8506262f87e2b127a7")
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = "apis", achromatic = "all", relative = FALSE, vonkries = TRUE), space = "hexagon"), n = c(1, 2), achromatic = TRUE, subset = c("Hibbertia_acicularis", "Grevillea_buxifolia")), digits = 4), 
               "754c01809100bdacc80d40db2359797f41180c23")
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = "segment")), achromatic = TRUE), digits = 4), 
               "d65c018342664ae9c8dca35e715c57dde28de30a")
})

test_that("bootcoldist", {
  library(digest)
  # set.seed(11023)
  data(sicalis)

  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))
  # expect_equal(digest::sha1(bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, cores = 1)), "67116b67a619e14189e65807a039be1fac6a4077")
  expect_equal(dim(bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, cores = 1)), c(3, 6))
})
