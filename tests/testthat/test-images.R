library(pavo)
context("adjacency")

test_that("as.rimg", {
  
  attributesuite <- function(x) {
    expect_equal(class(x), c("rimg", "array"))
    expect_equal(attr(x, "state"), "raw")
    expect_equal(attr(x, "imgname"), 'image')
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
  rimgfake <- as.rimg(imgfake, 'image')
  
  imggrey <- imgfake[,,1]
  rimggrey <- as.rimg(imggrey, 'image')

  imgfake2 <- list(imgfake, imgfake)
  rimgfake2 <- as.rimg(imgfake2, c('image', 'image'))

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
  expect_equivalent(unlist(attributes(rimgfake2)), c('rimg', 'list', 'raw'))
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
  
  imgfakes <- as.rimg(list(imgfake, imgfake), name = c('fake_01', 'fake_02'))
  
  # Single
  fake_class <- classify(imgfake, kcols = 2)
  expect_equal(dim(fake_class), c(10, 10))
  expect_true(is.rimg(fake_class))
  
  ## Multiple
  fake_IDs <- data.frame(
    ID = c("fake_02.png", "fake_01.jpg"),
    k = c(2, 2)
  )
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
  
  imgfakes <- as.rimg(list(imgfake, imgfake), name = c('fake_01', 'fake_02'))
  
  img1col <- as.rimg(array(c(
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100)
  ),
  dim = c(100, 100, 3)
  ))
  
  # Single
  fake_class <- classify(imgfake, kcols = 2)
  
  fake_adjacent <- adjacent(fake_class, xpts = 100, xscale = 10)
  expect_equal(fake_adjacent$k, 2)
  expect_equal(fake_adjacent$E_1_2, 2 * fake_adjacent$n_off * fake_adjacent$p_1 * fake_adjacent$p_2)
  expect_equal(round(fake_adjacent$p_1, 1), round(fake_adjacent$p_2, 1))
  expect_equal(fake_adjacent$A, (fake_adjacent$m_r / fake_adjacent$m_c))
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_equal(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))
  
  # Multiple
  fake2_class <- classify(imgfakes, kcols = 2)
  fake2_adjacent <- adjacent(fake2_class, xpts = 250, xscale = 150)
  expect_equal(fake2_adjacent$k, c(2, 2))
  expect_equal(fake2_adjacent$E_1_2, 2 * fake2_adjacent$n_off * fake2_adjacent$p_1 * fake2_adjacent$p_2)
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

})