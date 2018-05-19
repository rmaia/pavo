library(pavo)
context("adjacency")

test_that("Basic", {
  # set.seed(1)
  # # Single
  fakeimg <- array(c(
    as.matrix(rep(c(0.2, 0.4, 0.6), each = 250)),
    as.matrix(rep(c(0.4, 0.7, 0.8), each = 250)),
    as.matrix(rep(c(0.6, 0.1, 0.2), each = 250))),
    dim = c(750, 750, 3))
  # stripe <- getimg(system.file("testdata/images/validation/stripe.png", package = "pavo"))
  # stripe_class <- classify(stripe, n_cols = 4, ref_ID = 1)
  # stripe_adjacent <- adjacent(stripe_class, x_pts = 100, x_scale = 10, bkg_ID = c(1, 3), bkg_include = FALSE)
  # 
  # expect_equal(stripe_adjacent$k, 2)
  # expect_equal(stripe_adjacent$E_2_4, 2 * stripe_adjacent$n_off * stripe_adjacent$p_2 * stripe_adjacent$p_4)
  # expect_equal(round(stripe_adjacent$p_2, 1), round(stripe_adjacent$p_4, 1))
  # expect_equal(stripe_adjacent$A, (stripe_adjacent$m_r / stripe_adjacent$m_c))
  # expect_gt(stripe_adjacent$m_r, stripe_adjacent$m_c)
  # expect_gt(stripe_adjacent$N, stripe_adjacent$n_off)
  # expect_equal(stripe_adjacent$m, ((stripe_adjacent$m_r + stripe_adjacent$m_c) / 2))
  # 
  # # Multiple
  # snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
  # snakes_class <- classify(snakes, n_cols = 3, ref_ID = 1)
  # snakes_adj <- adjacent(snakes_class, x_pts = 250, x_scale = 150)
  
})

