context("colspace")

test_that("Receptor orders/names", {
  data(flowers)

  # dichromat
  di <- sensmodel(c(440, 330))
  names(di) <- c("wl", "l", "s")
  di.vis <- vismodel(flowers, visual = di)
  di.space <- colspace(di.vis)
  expect_equal(di.vis[, 1:2], di.space[, 2:1], ignore_attr = TRUE)

  expect_equal(
    di.space[, 1:4],
    dispace(di.vis),
    ignore_attr = TRUE
  )

  # trichromat
  tri <- sensmodel(c(550, 440, 330))
  names(tri) <- c("wl", "l", "m", "s")
  tri.vis <- vismodel(flowers, visual = tri)
  tri.space <- colspace(tri.vis)
  expect_equal(tri.vis[, 1:3], tri.space[, 3:1], ignore_attr = TRUE)

  expect_equal(
    tri.space[, 1:7],
    trispace(tri.vis),
    ignore_attr = TRUE
  )

  # tetrachromat
  tetra <- sensmodel(c(660, 550, 440, 330))
  names(tetra) <- c("wl", "l", "m", "s", "u")
  tetra.vis <- vismodel(flowers, visual = tetra)
  tetra.space <- colspace(tetra.vis)
  expect_equal(tetra.vis[, 1:4], tetra.space[, 4:1], ignore_attr = TRUE)
  expect_warning({
    sumtcs <- summary(tetra.space, by = 3)
  })

  expect_equal(
    tetra.space[, 1:16],
    tcspace(tetra.vis),
    ignore_attr = TRUE
  )

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

test_that("Errors/messages", {
  data(flowers)

  # Categorical
  expect_error(colspace(vismodel(flowers, visual = "apis"), space = "categorical"), "tetrachromatic")
  expect_warning(colspace(vismodel(flowers, visual = sensmodel(c(300, 400, 500, 600, 700))), space = "categorical"), "first four")
  expect_warning(colspace(vismodel(flowers, visual = sensmodel(c(300, 400, 500, 600, 700))), space = "categorical"), "not find")
  expect_warning(colspace(vismodel(flowers, visual = "musca", relative = FALSE), space = "categorical"), "relative")

  vis.flowers <- vismodel(flowers, visual = "musca")
  class(vis.flowers) <- "data.frame"
  expect_warning(colspace(vis.flowers, space = "categorical"), "vismodel")
  # expect_warning(colspace(cbind(vis.flowers, vis.flowers[,3]), space = 'categorical'), 'vismodel')
  # expect_warning(colspace(cbind(vis.flowers, vis.flowers[,3]), space = 'categorical'), 'undefined')
  expect_error(colspace(vis.flowers[1:3], space = "categorical"), "fewer")

  # Segment
  vis.flowers <- vismodel(flowers, visual = "segment")
  names(vis.flowers)[1] <- "Nada"
  expect_warning(colspace(vis.flowers, space = "segment"), "named")
  expect_error(colspace(vismodel(flowers, visual = "apis"), space = "segment"), "tetrachromatic")
  expect_warning(colspace(vismodel(flowers, visual = "segment", relative = FALSE), space = "segment"), "overriding")

  vis.flowers <- vismodel(flowers, visual = "segment")
  class(vis.flowers) <- "data.frame"
  expect_error(colspace(vis.flowers[1:3], space = "segment"), "fewer")
  expect_warning(colspace(vis.flowers, space = "segment"), "vismodel")
  expect_warning(colspace(vis.flowers, space = "segment"), "transformed")

  # Coc
  expect_error(colspace(vismodel(flowers, visual = "canis"), space = "coc"), "trichromatic")
  fak <- sensmodel(c(300, 400, 500, 600))
  expect_warning(colspace(vismodel(flowers, visual = fak, relative = FALSE, qcatch = "Ei", vonkries = TRUE), space = "coc"), "first")
  expect_warning(colspace(vismodel(flowers, visual = fak, relative = FALSE, qcatch = "Ei", vonkries = TRUE), space = "coc"), "trichromatic")
  expect_error(colspace(vismodel(flowers, visual = "apis", relative = TRUE, qcatch = "Ei", vonkries = TRUE), space = "coc"), "relative")
  expect_error(colspace(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Qi", vonkries = TRUE), space = "coc"), "hyperbolically")
  expect_error(colspace(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", vonkries = FALSE), space = "coc"), "von-Kries")

  vis.flowers <- vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", vonkries = TRUE)
  class(vis.flowers) <- "data.frame"

  expect_error(colspace(vis.flowers[1:2], space = "coc"), "fewer than three")
  expect_warning(colspace(vis.flowers[1:3], space = "coc"), "treating columns as")
  expect_warning(colspace(cbind(vis.flowers, vis.flowers[, 2]), space = "coc"), "has more than three")

  vis.flowers <- vismodel(flowers, visual = "apis", relative = TRUE, qcatch = "Ei", vonkries = TRUE)
  class(vis.flowers) <- "data.frame"
  expect_error(colspace(vis.flowers, space = "coc"), "relative")

  # Hexagon
  expect_error(colspace(vismodel(flowers, visual = "canis"), space = "hexagon"), "trichromatic")
  fak <- sensmodel(c(300, 400, 500, 600))
  expect_error(colspace(vismodel(flowers, visual = "apis", relative = TRUE), space = "hexagon"), "relative")
  expect_warning(colspace(vismodel(flowers, visual = fak, relative = FALSE), space = "hexagon"), "first three")
  expect_warning(colspace(vismodel(flowers, visual = "apis", relative = FALSE, vonkries = FALSE), space = "hexagon"), "hyperbolically")
  expect_warning(colspace(vismodel(flowers, visual = "apis", relative = FALSE, vonkries = FALSE), space = "hexagon"), "von-Kries")

  vis.flowers <- vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", vonkries = TRUE)
  class(vis.flowers) <- "data.frame"
  names(vis.flowers)[1] <- "a"

  expect_error(colspace(vis.flowers[1:2], space = "hexagon"), "fewer than three")
  expect_warning(colspace(vis.flowers[1:3], space = "hexagon"), "treating columns as")
  expect_warning(colspace(cbind(vis.flowers, vis.flowers[, 2]), space = "hexagon"), "has more than three")

  vis.flowers <- vismodel(flowers, visual = "apis", relative = TRUE, qcatch = "Ei", vonkries = TRUE)
  class(vis.flowers) <- "data.frame"
  expect_error(colspace(vis.flowers, space = "hexagon"), "relative")

  # tcs
  vis.flowers <- vismodel(flowers, visual = "apis")
  expect_error(colspace(vis.flowers, space = "tcs"), "not tetrachromatic")
  expect_warning(colspace(vismodel(flowers, visual = sensmodel(c(300, 400, 500, 600, 700))), space = "tcs"), "not tetrachromatic")
  class(vis.flowers) <- "data.frame"
  expect_error(colspace(vis.flowers[1:3], space = "tcs"), "has fewer than four")
  expect_message(colspace(vis.flowers, space = "tcs"), "treating columns as")
  expect_message(colspace(cbind(vis.flowers, vis.flowers[1:2]), space = "tcs"), "has more than four columns")
  expect_error(summary(colspace(vismodel(flowers)), by = 11), "not a multiple")

  vis.flowers <- vismodel(flowers, visual = "bluetit")
  names(vis.flowers) <- c("a", "b", "c", "d", "e")
  expect_warning(colspace(vis.flowers, space = "tcs"), "Could not find columns")

})

test_that("CIE", {

  data(flowers)
  vis_flowers <- colspace(vismodel(flowers, "cie10"))
  implicit_cie_flowers <- colspace(vis_flowers)
  expect_s3_class(implicit_cie_flowers, "colspace")
  explicit_cie_flowers <- colspace(vis_flowers, space = "ciexyz")
  expect_identical(implicit_cie_flowers, explicit_cie_flowers)

  # BYO data
  fakedat <- data.frame(X = c(0.1, 0.2),
                        Y = c(0.3, 0.4),
                        Z = c(0.5, 0.6),
                        lum = c(NA, NA))
  expect_equal(colspace(fakedat, space = "cielab", visual = sensdata(visual = 'cie10'), illum = sensdata(illum = 'D65')),
               colspace(fakedat, space = "cielab", visual = 'cie10', illum = 'D65'))

  # Should ignore custom options when data are class vismodel()
  expect_equal(colspace(vismodel(flowers, "cie10"), space = "cielab", visual = 'cie10', illum = 'D65'),
               colspace(vismodel(flowers, "cie10"), space = "cielab", visual = 'cie2', illum = 'bluesky'))

  # Message about the use of user-defined data
  expect_message(colspace(fakedat, space = "cielab", illum = sensdata(illum = 'D65')), 'custom illuminant')
  expect_message(colspace(fakedat, space = "cielab", visual = sensdata(visual = 'cie10')), 'custom visual system')

})
