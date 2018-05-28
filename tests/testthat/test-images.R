library(pavo)
context("adjacency")

test_that("Basic", {

  ## Single image
  fake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(0, 0, 1, 1), nrow = 100, ncol = 100)),
    dim = c(100, 100, 3)))
  fake_class <- classify(fake, kcols = 2)
  fake_adjacent <- adjacent(fake_class, xpts = 100, xscale = 10)
  expect_equal(fake_adjacent$k, 2)
  expect_equal(fake_adjacent$E_1_2, 2 * fake_adjacent$n_off * fake_adjacent$p_1 * fake_adjacent$p_2)
  expect_equal(round(fake_adjacent$p_1, 1), round(fake_adjacent$p_2, 1))
  expect_equal(fake_adjacent$A, (fake_adjacent$m_r / fake_adjacent$m_c))
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_equal(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))
   
  ## Multiple images
  fake2 <- list(fake, fake)
  attr(fake2[[1]], 'imgname') <- 'fake_01'
  attr(fake2[[2]], 'imgname') <- 'fake_02'
  fake_IDs <- data.frame(ID = c('fake_02.png', 'fake_01.jpg'),
                         k = c(2, 2))
  fake2_class <- classify(fake2, kcols = fake_IDs)
  fake2_class2 <- classify(fake2, kcols = fake_IDs, refID = 1)
  
  fake2_adjacent <- adjacent(fake2_class, xpts = 250, xscale = 150)
  expect_equal(fake2_adjacent$k, c(2, 2))
  expect_equal(fake2_adjacent$E_1_2, 2 * fake2_adjacent$n_off * fake2_adjacent$p_1 * fake2_adjacent$p_2)
  expect_equal(round(fake2_adjacent$p_1, 1), round(fake2_adjacent$p_2, 1))
  expect_equal(fake2_adjacent$A, (fake2_adjacent$m_r / fake2_adjacent$m_c))
  expect_gt(fake2_adjacent$N[1], fake2_adjacent$n_off[1])
  expect_equal(fake2_adjacent$m, ((fake2_adjacent$m_r + fake2_adjacent$m_c) / 2))
  
  ## Single colour
  fake3 <- as.rimg(array(c(
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(1, 1, 1, 1), nrow = 100, ncol = 100)),
    dim = c(100, 100, 3)))
  fake3_class <- classify(fake, kcols = 1)
  fake3_adjacent <- adjacent(fake3_class, xpts = 100, xscale = 10)
  expect_equal(fake3_adjacent$k, 1)
  expect_equal(fake3_adjacent$p_1, 1)
  expect_equal(fake3_adjacent$q_1_1, 1)
  expect_equal(fake3_adjacent$q_1_1, 1)
  expect_equal(fake3_adjacent$m, 0)
  expect_equal(fake3_adjacent$Sc, 1)
  
})

