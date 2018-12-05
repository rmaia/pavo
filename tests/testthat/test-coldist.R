library(pavo)
context("coldist")

test_that("Errors", {
  data(flowers)

  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3, 4, 5), achro = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3), achro = FALSE), "different length")
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
  
  vis.flowers <- vismodel(flowers, visual = 'apis')
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  
  expect_equal(dim(jnd2xyz(cd.flowers)), c(36, 2))
  expect_equal(dim(coldist(vismodel(flowers))), c(630, 3))
  
})

test_that("bootcoldist", {
  library(digest)
  data(sicalis)
  
  vm <- vismodel(sicalis, visual = 'apis', achro = 'all')
  gr <- gsub("ind..", "", rownames(vm))
  expect_equal(digest::sha1(bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, cores = 1)), "f0ab08ddf3fef952dda9529d3d13b9795c8a42b3")
  
})
  
