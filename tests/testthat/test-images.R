library(ptrn)
context("adjacency")

test_that("Basic", {
  # Single
  stripe <- getimg(system.file("testdata/images/stripe.png", package = "pavo"))
  stripe_class <- classify(stripe, n_cols = 2)
  stripe_adjacent <- adjacent(stripe_class, x_pts = 100, x_scale = 10)

  expect_equal(stripe_adjacent$k, 2)
  expect_equal(stripe_adjacent$E_1_2, 2 * stripe_adjacent$n_off * stripe_adjacent$p_1 * stripe_adjacent$p_2)
  expect_equal(round(stripe_adjacent$p_1, 1), round(stripe_adjacent$p_2, 1))
  expect_equal(stripe_adjacent$A, (stripe_adjacent$m_r / stripe_adjacent$m_c))
  expect_gt(stripe_adjacent$m_r, stripe_adjacent$m_c)
  expect_gt(stripe_adjacent$N, stripe_adjacent$n_off)
  expect_equal(stripe_adjacent$m, ((stripe_adjacent$m_r + stripe_adjacent$m_c) / 2))

  # Multiple
  snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
  snakes_class <- classify(snakes, n_cols = 3)
  snakes_adj <- adjacent(snakes_class, x_pts = 250, x_scale = 150)
  
})
