library(pavo)
context("colspace")

test_that("Receptor orders/names", {
  data(flowers)

  # dichromat
  di <- sensmodel(c(440, 330))
  names(di) <- c("wl", "l", "s")
  di.vis <- vismodel(flowers, visual = di)
  di.space <- colspace(di.vis)
  expect_equal(di.vis, di.space[, 2:1], check.attributes = FALSE)

  # trichromat
  tri <- sensmodel(c(550, 440, 330))
  names(tri) <- c("wl", "l", "m", "s")
  tri.vis <- vismodel(flowers, visual = tri)
  tri.space <- colspace(tri.vis)
  expect_equal(tri.vis, tri.space[, 3:1], check.attributes = FALSE)

  # tetrachromat
  tetra <- sensmodel(c(660, 550, 440, 330))
  names(tetra) <- c("wl", "l", "m", "s", "u")
  tetra.vis <- vismodel(flowers, visual = tetra)
  tetra.space <- colspace(tetra.vis)
  expect_equal(tetra.vis, tetra.space[, 4:1], check.attributes = FALSE)
})

test_that("Relative quantum catches", {
  data(flowers)

  # dichromat
  di <- sensmodel(c(440, 330))
  names(di) <- c("wl", "l", "s")

  di_vis <- vismodel(flowers, visual = di)
  di_vis_norel <- vismodel(flowers, visual = di, relative = FALSE)
  di_vis_noreldf <- as.data.frame(di_vis_norel)

  expect_warning(colspace(di_vis_norel), "not relative")
  expect_warning(colspace(di_vis_noreldf), "not relative")

  expect_equal(
    suppressWarnings(colspace(di_vis)),
    suppressWarnings(colspace(di_vis_norel))
  )

  # trichromat
  tri <- sensmodel(c(550, 440, 330))
  names(tri) <- c("wl", "l", "m", "s")

  tri_vis <- vismodel(flowers, visual = tri)
  tri_vis_norel <- vismodel(flowers, visual = tri, relative = FALSE)
  tri_vis_noreldf <- as.data.frame(tri_vis_norel)

  expect_warning(colspace(tri_vis_norel), "not relative")
  expect_warning(colspace(tri_vis_noreldf), "not relative")

  expect_equal(
    suppressWarnings(colspace(tri_vis)),
    suppressWarnings(colspace(tri_vis_norel))
  )
})

test_that("Output regression", {
  library(digest)
  data(flowers)
  
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'canis', achromatic = 'all'))), "7d024dad7f1feced295559d7fd444943e180e210")  # dispace
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'apis', achromatic = 'l'))), "2e4866f9d317995124830657a4d53d7041a5443c")  # trispace
  #expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'bluetit', achromatic = 'ch.dc'))), "e8c76d5a7665067b14b6822d151ae92f80b640ee")  # tcs
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'musca', achro = 'md.r1'), space = 'categorical')), "a7ccd08c94ebdb45001b54a15eb8263cd5b2550a")  # categorical
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'segment', achromatic = 'bt.dc'), space = 'segment')), "7c2b932002f772b91a310c14c71e4a60a00cdd2d")  # segment
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'apis', relative = FALSE, qcatch = 'Ei', vonkries = TRUE, achromatic = 'l'), space = 'coc')), "8aff94ca6f00a16aa1b2a462bdad814fb8ebcaa1")  # coc
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'apis', qcatch = 'Ei', vonkries = TRUE, relative = FALSE, achromatic = 'l'), space = 'hexagon')), "8d78ae80e697548ddf30f6bb906b6162172375f2")  # hexagon
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'cie10'), space = 'ciexyz')), "959b29494ec390d964118e217f59d3ba86c47a1e")  # ciexyz
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'cie10'), space = 'cielab')), "9858e3ac93193dd0e8aa792b1deac2db8df9171a")  # cielab
  expect_equal(digest::sha1(colspace(vismodel(flowers, visual = 'cie10'), space = 'cielch')), "1146137e125167d95410a4316deca286f7dc9727")  # cielch
 

})