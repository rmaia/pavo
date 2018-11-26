library(pavo)
context("vismodel")

test_that("Output is in expected range", {
  data(flowers)
  red <- flowers[c("wl", "Goodenia_gracilis")] # A really red flower

  # Test expected rank-order photoreceptor stimulation w/ 'red' flower under a few different conditions/viewers
  # Just an idea - surely a better way to do this
  m.hex <- vismodel(red, relative = FALSE, qcatch = "Ei", vonkries = TRUE, visual = "apis")
  expect_true(m.hex$l > m.hex$m && m.hex$m > m.hex$s)

  m.tcs <- vismodel(red, relative = FALSE, qcatch = "fi", visual = "bluetit", scale = 10000)
  expect_true(m.tcs$l > m.tcs$m && m.tcs$m > m.tcs$s && m.tcs$s > m.tcs$u)

  m.di <- vismodel(red, relative = FALSE, qcatch = "fi", visual = "canis", scale = 10000)
  expect_true(m.di$l > m.di$s)

  m.fly <- vismodel(red, relative = FALSE, visual = "musca", achro = "l", scale = 10000)
  expect_true(m.fly$l > m.fly$m && m.fly$m > m.fly$s && m.fly$s > m.fly$u && m.fly$lum == m.fly$l)
})

test_that("Warnings", {
  data(flowers)
  fakedat <- as.rspec(data.frame(wl = c(300:700), refl1 = rnorm(401), refl2 = rnorm(401)))

  expect_warning(vismodel(flowers, vonkries = FALSE, relative = FALSE, achro = "none", visual = "cie10"), "overriding vonkries")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = TRUE, achro = "none", visual = "cie10"), "overriding relative")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = FALSE, achro = "l", visual = "cie10"), "overriding achro")
  expect_warning(vismodel(flowers, qcatch = "fi", vonkries = TRUE, relative = FALSE, achro = "none", visual = "cie10"), "overriding qcatch")
  expect_warning(vismodel(fakedat, visual = "bluetit"), "negative")

  test_rspec <- as.rspec(flowers[1:2])
  test_matrix <- as.matrix(flowers[1:2])

  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_rspec), "Illuminant is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_matrix), "Illuminant is a matrix")
  expect_warning(vismodel(flowers, visual = "bluetit", achro = test_rspec), "Achromatic is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", achro = test_matrix), "Achromatic is a matrix")
  expect_silent(vismodel(flowers, visual = "bluetit", achro = FALSE))
})

test_that("Summary", {
  data(sicalis)
  vis.sicalis <- vismodel(sicalis, visual='avg.uv')
  summary(vis.sicalis)
  
  
})
