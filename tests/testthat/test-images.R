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
  expect_gt(fake_adjacent$m_r, fake_adjacent$m_c)
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_equal(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))
   
  ## Multiple images
  fake2 <- list(fake, fake)
  attr(fake2[[1]], 'imgname') <- 'fake_01'
  attr(fake2[[2]], 'imgname') <- 'fake_02'
  fake_IDs <- data.frame(ID = c('fake_02.png', 'fake_01.jpg'),
                         k = c(2, 2))
  fake_class <- classify(fake2, kcols = fake_IDs)
  fake_adj <- adjacent(fake_class, xpts = 250, xscale = 150)
  
})

