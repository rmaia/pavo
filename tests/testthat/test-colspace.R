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
  
    # expect_known_hash(colspace(vismodel(flowers, visual = 'canis', achromatic = 'all')), "f1527dab25")  
    # expect_known_hash(colspace(vismodel(flowers, visual = 'apis', achromatic = 'l')), "39aaa44796")  # trispace
    # expect_known_hash(colspace(vismodel(flowers, visual = 'bluetit', achromatic = 'ch.dc')), "e65fb49b00")  # tcs
    # expect_known_hash(colspace(vismodel(flowers, visual = 'musca', achro = 'md.r1'), space = 'categorical'), "10c6cd96db")  # categorical
    # expect_known_hash(colspace(vismodel(flowers, visual = 'segment', achromatic = 'bt.dc'), space = 'segment'), "1030bfa096")  # segment
    # expect_known_hash(colspace(vismodel(flowers, visual = 'apis', relative = FALSE, qcatch = 'Ei', vonkries = TRUE, achromatic = 'l'), space = 'coc'), "9cb10da64d")  # coc
    # expect_known_hash(colspace(vismodel(flowers, visual = 'apis', qcatch = 'Ei', vonkries = TRUE, relative = FALSE, achromatic = 'l'), space = 'hexagon'), "7cf8755d84")  # hexagon
    # expect_known_hash(colspace(vismodel(flowers, visual = 'cie10'), space = 'ciexyz'), "1faeee0d09")  # ciexyz
    # expect_known_hash(colspace(vismodel(flowers, visual = 'cie10'), space = 'cielab'), "7e80adf858")  # cielab
    # expect_known_hash(colspace(vismodel(flowers, visual = 'cie10'), space = 'cielch'), "beeb4c34b0")  # cielch

})