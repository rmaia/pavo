library(pavo)
context("adjacency")

test_that("as.rimg", {
  attributesuite <- function(x) {
    expect_equal(class(x), c("rimg", "array"))
    expect_equal(attr(x, "state"), "raw")
    expect_equal(attr(x, "imgname"), "image")
    expect_equal(attr(x, "px_scale"), NA)
    expect_equal(attr(x, "raw_scale"), NA)
    expect_equal(attr(x, "k"), NA)
    expect_equal(attr(x, "outline"), NA)
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

  imggrey <- imgfake[, , 1]
  rimggrey <- as.rimg(imggrey, "image")

  imgfake2 <- list(imgfake, imgfake)
  rimgfake2 <- as.rimg(imgfake2, c("image", "image"))

  # Warnings/messages
  expect_message(as.rimg(imgfake), "[0,1]")
  expect_message(as.rimg(imgfake2), "[0,1]")

  # Classes/attributes/structure
  expect_equal(dim(rimgfake), c(10, 10, 3))
  expect_equal(dim(rimggrey), c(10, 10, 3))
  expect_equal(dim(rimgfake2[[1]]), c(10, 10, 3))
  expect_equal(dim(rimgfake2[[2]]), c(10, 10, 3))
  attributesuite(rimgfake)
  attributesuite(rimggrey)
  attributesuite(rimgfake2[[1]])
  attributesuite(rimgfake2[[2]])
  expect_equivalent(unlist(attributes(rimgfake2)), c("rimg", "list", "raw"))
  expect_true(is.rimg(rimgfake))
  expect_true(is.rimg(rimgfake2))
  expect_true(is.rimg(rimggrey))
})

test_that("classify", {

  # Images
  imgfake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 1, 1), nrow = 10, ncol = 10)
  ),
  dim = c(10, 10, 3)
  ))

  imgfakes <- as.rimg(list(imgfake, imgfake), name = c("fake_01", "fake_02"))

  # Single
  expect_error(classify(1, kcols = fake_IDs), "array")

  fake_class <- classify(imgfake, kcols = 2)
  expect_equal(dim(fake_class), c(10, 10))
  expect_true(is.rimg(fake_class))

  ## Multiple
  fake_IDs <- data.frame(
    ID = c("fake_02.png", "fake_01.jpg"),
    k = c(2, 2)
  )

  expect_error(classify(list(1, 1), kcols = fake_IDs), "array")

  fake2_class <- classify(imgfakes, kcols = fake_IDs)
  expect_true(is.rimg(fake2_class))
  expect_true(is.rimg(fake2_class[[1]]))
  expect_true(is.rimg(fake2_class[[2]]))
  expect_equal(dim(fake2_class[[1]]), c(10, 10))
  expect_equal(dim(fake2_class[[2]]), c(10, 10))

  fake2_class2 <- classify(imgfakes, kcols = fake_IDs, refID = 1)
  expect_true(is.rimg(fake2_class2))
  expect_true(is.rimg(fake2_class2[[1]]))
  expect_true(is.rimg(fake2_class2[[2]]))
  expect_equal(dim(fake2_class2[[1]]), c(10, 10))
  expect_equal(dim(fake2_class2[[2]]), c(10, 10))
})

test_that("adjacency", {

  # Images
  imgfake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 0, 0), nrow = 10, ncol = 10),
    matrix(c(0, 0, 1, 1), nrow = 10, ncol = 10)
  ),
  dim = c(10, 10, 3)
  ))

  imgfakes <- as.rimg(list(imgfake, imgfake), name = c("fake_01", "fake_02"))

  img1col <- as.rimg(array(c(
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100)
  ),
  dim = c(100, 100, 3)
  ))
  
  distances <- data.frame(c1 = 1,
                          c2 = 2,
                          dS = 10,
                          dL = 1)
  distances2 <- data.frame(c1 = 'wrong',
                          c2 = 2,
                          dS = 10,
                          dL = 1)
  hsl_vals <- data.frame(patch = 1:2,
                         hue = c(1.4, 0.2),
                         lum = c(10, 5),
                         sat = c(3.5, 1.1))

  # Single
  fake_class <- classify(imgfake, kcols = 2)
  expect_error(adjacent(10, xpts = 10, xscale = 10), "array")
  expect_error(adjacent(fake_class, xpts = 10, xscale = 10, coldists = distances2), "do not match")
  
  fake_adjacent <- adjacent(fake_class, xpts = 10, xscale = 10, coldists = distances, hsl = hsl_vals, bkgID = 1)
  expect_message(adjacent(fake_class, xpts = 123, xscale = 10), "grid-sampling density")
  expect_equal(fake_adjacent$k, 2)
  expect_equal(fake_adjacent$m_dS, 10)
  expect_equal(fake_adjacent$m_dL, 1)
  expect_equal(fake_adjacent$cv_sat, fake_adjacent$s_sat / fake_adjacent$m_sat)
  expect_equal(fake_adjacent$cv_lum, fake_adjacent$s_lum / fake_adjacent$m_lum)
  expect_equal(round(fake_adjacent$p_1, 1), round(fake_adjacent$p_2, 1))
  expect_equal(fake_adjacent$A, (fake_adjacent$m_r / fake_adjacent$m_c))
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_equal(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))

  # Multiple
  fake2_class <- classify(imgfakes, kcols = 2)
  expect_error(adjacent(list(10, 10), xpts = 10, xscale = 10), "array")

  fake2_adjacent <- adjacent(fake2_class, xpts = 10, xscale = 150)
  expect_message(adjacent(fake2_class, xpts = 123, xscale = 10), "grid-sampling density")
  expect_equal(fake2_adjacent$k, c(2, 2))
  expect_equal(round(fake2_adjacent$p_1, 1), round(fake2_adjacent$p_2, 1))
  expect_equal(fake2_adjacent$A, (fake2_adjacent$m_r / fake2_adjacent$m_c))
  expect_gt(fake2_adjacent$N[1], fake2_adjacent$n_off[1])
  expect_equal(fake2_adjacent$m, ((fake2_adjacent$m_r + fake2_adjacent$m_c) / 2))

  ## Single colour
  fake3_class <- classify(img1col, kcols = 1)
  fake3_adjacent <- adjacent(fake3_class, xpts = 100, xscale = 10)
  expect_equal(fake3_adjacent$k, 1)
  expect_equal(fake3_adjacent$p_1, 1)
  expect_equal(fake3_adjacent$q_1_1, 1)
  expect_equal(fake3_adjacent$q_1_1, 1)
  expect_equal(fake3_adjacent$m, 0)
  expect_equal(fake3_adjacent$Sc, 1)
  
  ## Checkerboard (known values)
  distances <- data.frame(c1 = 1,
                          c2 = 2,
                          dS = 10,
                          dL = 20)
  
  hsl_vals <- data.frame(patch = 1:2,
                         hue = c(1, 2),
                         lum = c(1, 10),
                         sat = c(1, 6))
  set.seed(1)
  checker <- getimg(system.file("testdata/images/validation/checkerboard.png", package = 'pavo'))
  checker_class <- classify(checker, kcols = 2)
  checker_adj <- adjacent(checker_class, xscale = 10, xpts = 220, coldists = distances, hsl = hsl_vals)
  expect_equal(checker_adj$k, 2)
  expect_equal(checker_adj$N, (219*220)*2)
  expect_equal(checker_adj$p_2, 0.52)
  expect_equal(checker_adj$p_1, 0.48)
  expect_equal(checker_adj$t_1_2, 1)
  expect_equal(checker_adj$m, 0.4)
  expect_equal(checker_adj$m_r, 0.4)
  expect_equal(checker_adj$m_c, 0.4)
  expect_equal(checker_adj$A, 1)
  expect_equal(checker_adj$St, 1)
  expect_equal(checker_adj$Jt, 1)
  expect_equal(checker_adj$m_dS, 10)
  expect_equal(checker_adj$m_dL, 20)
  expect_equal(checker_adj$m_hue, 1.5)
  expect_equal(checker_adj$m_sat, 3.6)
  expect_equal(checker_adj$m_lum, 5.68)

})
