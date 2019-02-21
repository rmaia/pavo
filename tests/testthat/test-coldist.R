library(pavo)
context("coldist")

test_that("Errors", {
  data(flowers)

  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3, 4, 5), achro = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3), achro = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), subset = c('Goodenia', 'Xyris', "Eucalyptus"), achro = FALSE), "Too many")
  expect_error(coldist(as.matrix(vismodel(flowers)), noise = "quantum"), "quantum receptor noise model")
  expect_error(coldist(vismodel(flowers, qcatch = "Ei")), "not compatible with hyperbolically")
  expect_error(coldist(as.matrix(vismodel(flowers))), "quantum catches not defined")
  expect_error(coldist(vismodel(flowers), weber.ref = 5), "greater than the length of vector")

})

test_that("Warnings", {
  data(flowers)

  expect_warning(coldist(vismodel(flowers)), "Quantum catch are relative")
  expect_warning(coldist(vismodel(flowers), achromatic = TRUE), "achromatic contrast not calculated")
  expect_warning(coldist(as.matrix(vismodel(flowers)), qcatch = "Qi"), "number of cones not specified")
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
    coldist(vismodel(flowers, relative = FALSE), achromatic = FALSE),
    suppressWarnings(coldist(colspace(vismodel(flowers, relative = FALSE)), achro = FALSE)),
    check.attributes = FALSE
  )

  expect_equal(
    suppressWarnings(coldist(vismodel(flowers))),
    suppressWarnings(coldist(colspace(vismodel(flowers)))),
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

  vis.flowers <- vismodel(flowers, visual = 'apis')
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)

  expect_equal(digest::sha1(jndrot(jnd2xyz(coldist(vismodel(flowers, achromatic = 'bt.dc', relative = FALSE), achromatic = TRUE))), digits = 4), "1785819f0a27bd0fcd516ef3cdc6753612870d76")


  # Errors
  expect_equal(dim(jnd2xyz(cd.flowers)), c(36, 2))
  expect_equal(dim(coldist(vismodel(flowers))), c(630, 3))

  rownames(attr(jnd.flowers, 'resref'))[4] <- 'nope'
  expect_error(jndrot(jnd.flowers), 'does not match')

  class(jnd.flowers) <- 'data.frame'
  expect_error(jndrot(jnd.flowers), 'jnd2xyz')


})

test_that("Output regression", {
  library(digest)
  data(flowers)
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = 'canis', achromatic = 'all')), n = c(1, 2), achromatic = TRUE, subset = 'Hibbertia_acicularis'), digits = 4), "c965bc7dcee5fda9748f80113874b8f7ac7250be")  # dispace
  expect_equal(digest::sha1(coldist(colspace(vismodel(flowers, visual = 'apis', achromatic = 'all', relative = FALSE, vonkries = TRUE), space = 'hexagon'), n = c(1, 2), achromatic = TRUE, subset = c('Hibbertia_acicularis', 'Grevillea_buxifolia')), digits = 4), "d3fea6d99f61626581756c92e529a8a107a4411a")  # dispace
})

test_that("bootcoldist", {
  library(digest)
  #set.seed(11023)
  data(sicalis)

  vm <- vismodel(sicalis, visual = 'apis', achro = 'l')
  gr <- gsub("ind..", "", rownames(vm))
  #expect_equal(digest::sha1(bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, cores = 1)), "67116b67a619e14189e65807a039be1fac6a4077")
  expect_equal(dim(bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, cores = 1)), c(3, 6))

})
