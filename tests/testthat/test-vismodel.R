library(pavo)
context('Vismodel')

test_that('Output is in expected range', {
  data(flowers)
  red <- flowers[c('wl', 'Goodenia_gracilis')]  # A really red flower

  # Test expected rank-order photoreceptor stimulation w/ 'red' flower under a few different conditions/viewers
  # Just an idea - surely a better way to do this
  m.hex <- vismodel(red, relative = FALSE, qcatch = 'Ei', vonkries = TRUE, visual = 'apis')
  expect_true(m.hex$l > m.hex$m && m.hex$m > m.hex$s)
  
  m.tcs <- vismodel(red, relative = FALSE, qcatch = 'fi', visual = 'bluetit', scale = 10000)
  expect_true(m.tcs$l > m.tcs$m && m.tcs$m > m.tcs$s && m.tcs$s > m.tcs$u)
  
  m.di <- vismodel(red, relative = FALSE, qcatch = 'fi', visual = 'canis', scale = 10000)
  expect_true(m.di$l > m.di$s)
  
  m.fly <- vismodel(red, relative = FALSE, visual = 'musca', achro = 'l', scale = 10000)
  expect_true(m.fly$l > m.fly$m && m.fly$m > m.fly$s && m.fly$s > m.fly$u && m.fly$lum == m.fly$l)
  
})