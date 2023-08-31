test_that("as.rimg", {
  attributesuite <- function(x) {
    expect_s3_class(x, c("rimg", "array"))
    expect_identical(attr(x, "state"), "raw")
    expect_identical(attr(x, "imgname"), "image")
    expect_identical(attr(x, "px_scale"), NA)
    expect_identical(attr(x, "raw_scale"), NA)
    expect_identical(attr(x, "k"), NA)
    expect_identical(attr(x, "outline"), NA)
  }

  # Fake image(s)
  imgfake <- array(c(
    matrix(c(155, 1, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 1, 1), nrow = 10, ncol = 10)
  ),
  dim = c(10, 10, 3)
  )
  rimgfake <- as.rimg(imgfake, "image")
  c <- rimg2magick(rimgfake)

  imggrey <- imgfake[, , 1]
  rimggrey <- as.rimg(imggrey, "image")

  imgfake2 <- list(imgfake, imgfake)
  rimgfake2 <- as.rimg(imgfake2, c("image", "image"))

  # Warnings/messages
  expect_message(as.rimg(imgfake), "[0,1]")
  expect_message(as.rimg(imgfake2), "[0,1]")

  # Classes/attributes/structure
  expect_identical(dim(rimgfake), c(10L, 10L, 3L))
  expect_identical(dim(rimggrey), c(10L, 10L, 3L))
  expect_identical(dim(rimgfake2[[1]]), c(10L, 10L, 3L))
  expect_identical(dim(rimgfake2[[2]]), c(10L, 10L, 3L))
  attributesuite(rimgfake)
  attributesuite(rimggrey)
  attributesuite(rimgfake2[[1]])
  attributesuite(rimgfake2[[2]])
  expect_equal(unlist(attributes(rimgfake2)), c("rimg", "list", "raw"), ignore_attr = TRUE)
  expect_true(is.rimg(rimgfake))
  expect_true(is.rimg(rimgfake2))
  expect_true(is.rimg(rimggrey))

  # as.rimg can handle user-classified images
  papilio <- getimg(system.file("testdata", "images", "butterflies", "papilio.png", package = "pavo"))
  papilio_class <- classify(papilio, kcols = 4)
  pap2 <- as.rimg(matrix(papilio_class, nrow = (nrow(papilio_class)), ncol = ncol(papilio_class)))
  expect_true(is.rimg(pap2))
  expect_equal(papilio_class, pap2, ignore_attr = TRUE)

  # magick conversion
  magickpapilio <- rimg2magick(papilio)
  expect_s3_class(magickpapilio, "magick-image")
  papiliomagick <- as.rimg(magickpapilio)
  attr(papiliomagick, "imgname") <- "papilio"
  expect_equal(papilio, papiliomagick, ignore_attr = TRUE)
})

test_that("procimg", {
  papilio <- getimg(system.file("testdata", "images", "butterflies", "papilio.png", package = "pavo"))

  # Resize
  expect_identical(dim(procimg(papilio, resize = 50))[1:2], dim(papilio)[1:2] %/% 2L)

  # Messages/Errors
  expect_message(procimg(classify(papilio, kcols = 3), resize = 200), "Cannot resize")
  expect_message(procimg(classify(papilio, kcols = 3), rotate = 90), "Cannot rotate")
  expect_error(procimg(papilio), "options")
  class(papilio) <- "array"
  expect_message(procimg(papilio, "coerce"))
  class(papilio) <- "cimg"
  expect_message(procimg(papilio, "coerce"))

  # Acuity
  expect_warning(procimg(papilio, obj_dist = 3), "Skipping")
  expect_warning(procimg(papilio, obj_width = 3), "Skipping")
  expect_warning(procimg(papilio, eye_res = 3), "Skipping")
  expect_warning(procimg(papilio, obj_width = 3, obj_dist = 3), "Skipping")
  expect_warning(procimg(papilio, obj_width = "black", obj_dist = 3, eye_res = 10), "Skipping")
  expect_warning(procimg(classify(papilio, kcols = 3), obj_width = 3, obj_dist = 3, eye_res = 10), "Skipping")
})


test_that("classify", {

  # Images
  imgfake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 12, ncol = 8),
    matrix(c(0, 0, 0, 0), nrow = 12, ncol = 8),
    matrix(c(0, 0, 1, 1), nrow = 12, ncol = 8)
  ),
  dim = c(12, 8, 3)
  ))

  imgfakes <- as.rimg(list(imgfake, imgfake), name = c("fake_01", "fake_02"))

  # Single
  expect_error(classify(1, kcols = 1), "array")
  expect_error(classify(imgfake), "kcols")

  fake_class <- classify(imgfake, kcols = 2)
  expect_identical(dim(fake_class), c(8L, 12L))
  expect_true(is.rimg(fake_class))

  # Shouldn't fail even when user unnecessarily specifies refID for single img
  # expect_identical(
  #   {
  #     set.seed(1)
  #     classify(imgfake, kcols = 2)
  #   },
  #   {
  #     set.seed(1)
  #     classify(imgfake, kcols = 2, refID = "img")
  #   }
  # )

  ## Multiple
  fake_IDs <- data.frame(
    ID = c("fake_02.png", "fake_01.jpg"),
    k = c(2, 2),
    stringsAsFactors = FALSE
  )

  expect_error(classify(list(1, 1), kcols = fake_IDs), "array")

  fake2_class <- classify(imgfakes, kcols = fake_IDs)
  expect_true(is.rimg(fake2_class))
  expect_true(is.rimg(fake2_class[[1]]))
  expect_true(is.rimg(fake2_class[[2]]))
  expect_identical(dim(fake2_class[[1]]), c(8L, 12L))
  expect_identical(dim(fake2_class[[2]]), c(8L, 12L))

  fake2_class2 <- classify(imgfakes, kcols = fake_IDs, refID = 1)
  expect_true(is.rimg(fake2_class2))
  expect_true(is.rimg(fake2_class2[[1]]))
  expect_true(is.rimg(fake2_class2[[2]]))
  expect_identical(dim(fake2_class2[[1]]), c(8L, 12L))
  expect_identical(dim(fake2_class2[[2]]), c(8L, 12L))

  # k medoids
  expect_error(classify(imgfakes, method = "kMedoids", kcols = fake_IDs, refID = 1), "k-medoids")
  expect_error(classify(imgfakes, method = "kMedoids", interactive = TRUE), "k-medoids")
  fake2_class3 <- classify(imgfakes, method = "kMedoids", kcols = fake_IDs)
  expect_true(is.rimg(fake2_class3))
  expect_true(is.rimg(fake2_class3[[1]]))
  expect_true(is.rimg(fake2_class3[[2]]))
  expect_identical(dim(fake2_class3[[1]]), c(8L, 12L))
  expect_identical(dim(fake2_class3[[2]]), c(8L, 12L))


  # Messages
  expect_error(classify(imgfakes, refID = "fail"), "No image found with that name")
  expect_error(classify(imgfakes, kcols = 10), "cluster centers exceeds the number of distinct data points")
  expect_message(classify(imgfakes, kcols = c(1, 2), refID = 1), "Cannot use reference image")
})

test_that("adjacency", {

  # Images
  imgfake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 12, ncol = 8),
    matrix(c(0, 0, 0, 0), nrow = 12, ncol = 8),
    matrix(c(0, 0, 1, 1), nrow = 12, ncol = 8)
  ),
  dim = c(12, 8, 3)
  ))

  imgfakes <- as.rimg(list(imgfake, imgfake), name = c("fake_01", "fake_02"))

  img1col <- as.rimg(array(c(
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100)
  ),
  dim = c(100, 100, 3)
  ))

  distances <- data.frame(
    c1 = 1,
    c2 = 2,
    dS = 10,
    dL = 1
  )
  distances2 <- data.frame(
    c1 = "wrong",
    c2 = 2,
    dS = 10,
    dL = 1,
    stringsAsFactors = FALSE
  )
  hsl_vals <- data.frame(
    patch = 1:2,
    hue = c(1.4, 0.2),
    lum = c(10, 5),
    sat = c(3.5, 1.1)
  )

  # Single
  set.seed(1234)
  fake_class <- classify(imgfake, kcols = 2)
  fake_class_90 <- classify(procimg(imgfake, rotate = 90), kcols = 2)
  expect_error(adjacent(10, xpts = 10, xscale = 10), "array")
  expect_error(adjacent(fake_class, xpts = 10, xscale = 10, coldists = distances2), "do not match")

  fake_adjacent <- adjacent(fake_class, xpts = 8, xscale = 10, coldists = distances, hsl = hsl_vals, bkgID = 1)
  fake_adjacent_90 <- adjacent(fake_class_90, xpts = 8, xscale = 10, coldists = distances, hsl = hsl_vals, bkgID = 1)
  expect_message(adjacent(fake_class, xpts = 123, xscale = 10), "grid-sampling density")
  expect_gt(fake_adjacent$m_r, fake_adjacent_90$m_c)
  expect_identical(fake_adjacent[, ncol(fake_adjacent):(ncol(fake_adjacent) - 8)], fake_adjacent_90[, ncol(fake_adjacent):(ncol(fake_adjacent) - 8)])
  expect_identical(fake_adjacent$k, 2L)
  expect_identical(fake_adjacent$m_dS, 10)
  expect_identical(fake_adjacent$m_dL, 1)
  expect_identical(fake_adjacent$cv_sat, fake_adjacent$s_sat / fake_adjacent$m_sat)
  expect_identical(fake_adjacent$cv_lum, fake_adjacent$s_lum / fake_adjacent$m_lum)
  expect_identical(round(fake_adjacent$p_1, 1), round(fake_adjacent$p_2, 1))
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_identical(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))

  # Multiple
  fake2_class <- classify(imgfakes, kcols = 2)
  expect_error(adjacent(list(10, 10), xpts = 10, xscale = 10), "array")

  fake2_adjacent <- adjacent(fake2_class, xpts = 10, xscale = 150)
  expect_message(adjacent(fake2_class, xpts = 123, xscale = 10), "grid-sampling density")
  expect_identical(fake2_adjacent$k, c(2L, 2L))
  expect_identical(fake2_adjacent$p_1, fake2_adjacent$p_2)
  expect_gt(fake2_adjacent$N[1], fake2_adjacent$n_off[1])
  expect_identical(fake2_adjacent$m, ((fake2_adjacent$m_r + fake2_adjacent$m_c) / 2))

  ## Single colour
  fake3_class <- classify(img1col, kcols = 1)
  fake3_adjacent <- adjacent(fake3_class, xpts = 100, xscale = 10)
  expect_identical(fake3_adjacent$k, 1L)
  expect_identical(fake3_adjacent$p_1, 1)
  expect_identical(fake3_adjacent$q_1_1, 1)
  expect_identical(fake3_adjacent$q_1_1, 1)
  expect_identical(fake3_adjacent$m, 0)
  expect_identical(fake3_adjacent$Sc, 1)

  ## Checkerboard (known values)
  distances <- data.frame(
    c1 = 1,
    c2 = 2,
    dS = 10,
    dL = 20
  )

  hsl_vals <- data.frame(
    patch = 1:2,
    hue = c(1, 2),
    lum = c(1, 10),
    sat = c(1, 6)
  )
  set.seed(1)
  checker <- getimg(system.file("testdata", "images", "validation", "checkerboard.png", package = "pavo"))
  checker_class <- classify(checker, kcols = 2)
  checker_adj <- adjacent(checker_class, xscale = 10, xpts = 220, coldists = distances, hsl = hsl_vals)
  expect_identical(checker_adj$k, 2L)
  expect_identical(checker_adj$N, (219L * 220L) * 2L)
  expect_identical(checker_adj$p_2, 0.52)
  expect_identical(checker_adj$p_1, 0.48)
  expect_identical(checker_adj$t_1_2, 1)
  expect_identical(checker_adj$m, 0.4)
  expect_identical(checker_adj$m_r, 0.4)
  expect_identical(checker_adj$m_c, 0.4)
  expect_identical(checker_adj$A, 1)
  expect_identical(checker_adj$St, 1)
  expect_identical(checker_adj$Jt, 1)
  expect_identical(checker_adj$m_dS, 10)
  expect_identical(checker_adj$A, (checker_adj$m_r / checker_adj$m_c))
  expect_identical(checker_adj$m_dL, 20)
  expect_identical(checker_adj$m_hue, 1.5)
  expect_identical(checker_adj$m_sat, 3.6)
  expect_identical(checker_adj$m_lum, 5.68)

  # Can handle user-classified images
  papilio <- getimg(system.file("testdata", "images", "butterflies", "papilio.png", package = "pavo"))
  papilio_class <- classify(papilio, kcols = 4)
  papilio_adj <- adjacent(papilio_class, xscale = 100)

  pap2 <- as.rimg(matrix(papilio_class, nrow = (nrow(papilio_class)), ncol = ncol(papilio_class)), name = "papilio")
  pap2_adj <- adjacent(pap2, xscale = 100)

  expect_identical(papilio_adj, pap2_adj)
})

# test_that("summary", {
#   suppressWarnings(RNGversion("3.5.0")) # back compatibility for now
#   set.seed(2231)
#
#   papilio <- getimg(system.file("testdata/images/butterflies/papilio.png", package = "pavo"))
#   papilio_class <- classify(papilio, kcols = 4)
#   snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#   snakes_class <- classify(snakes, kcols = 3)
#
#   expect_equal(summary(papilio_class)[1:3], data.frame(rep("papilio", 4), 1:4, 1:4, stringsAsFactors = FALSE), ignore_attr = TRUE)
#
#   truth <- c(3.621, 1.827, 0.159)
#   expect_equal(colSums(round(summary(snakes_class)[4:6], 3)), truth, ignore_attr = TRUE)
# })
