library(pavo)
context("adjacency")

test_that("Basic", {

  # # Single
  fake <- as.rimg(array(c(
    matrix(c(1, 1, 0, 0), nrow = 100, ncol = 100),
    matrix(c(0, 0, 0, 0), nrow = 100, ncol = 100),
    matrix(c(0, 0, 1, 1), nrow = 100, ncol = 100)),
    dim = c(100, 100, 3)))
  fake_class <- classify(fake, n_cols = 2)
  fake_adjacent <- adjacent(fake_class, x_pts = 100, x_scale = 10)
  expect_equal(fake_adjacent$k, 2)
  expect_equal(fake_adjacent$E_1_2, 2 * fake_adjacent$n_off * fake_adjacent$p_1 * fake_adjacent$p_2)
  expect_equal(round(fake_adjacent$p_1, 1), round(fake_adjacent$p_2, 1))
  expect_equal(fake_adjacent$A, (fake_adjacent$m_r / fake_adjacent$m_c))
  expect_gt(fake_adjacent$m_r, fake_adjacent$m_c)
  expect_gt(fake_adjacent$N, fake_adjacent$n_off)
  expect_equal(fake_adjacent$m, ((fake_adjacent$m_r + fake_adjacent$m_c) / 2))
   
  # # Multiple
  fake2 <- list(fake, fake)
  fake_class <- classify(fake, n_cols = 2, ref_ID = 1)
  fake_adj <- adjacent(fake_class, x_pts = 250, x_scale = 150)
  
})

