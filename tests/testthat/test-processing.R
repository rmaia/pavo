library(pavo)
context('processing')

test_that('Conversion to rspec', {
  fakedat <- data.frame(wl = seq(200, 800, 0.5), refl1 = rnorm(1201), refl2 = rnorm(1201))
  
  expect_equal(dim(as.rspec(fakedat, lim = c(300, 700), interp = FALSE)), c(801, 3))
  expect_equal(dim(as.rspec(fakedat, lim = c(200, 800), interp = FALSE)), c(1201, 3))
  expect_equal(dim(as.rspec(fakedat, lim = c(300, 700), interp = TRUE)), c(401, 3))
  expect_equal(as.data.frame(as.rspec(fakedat, lim = c(300, 700))['wl']), data.frame(wl = as.numeric(c(300:700))))
  
  expect_error(as.rspec(fakedat, lim = c(300.1, 700), interp = FALSE), "limits")
  expect_error(as.rspec(fakedat, lim = c(300, 699.1), interp = FALSE), "limits")
  
})