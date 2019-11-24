context("vismodel")

test_that("Output regression", {
  library(digest)
  data(flowers)

  # Output
  expect_equal(digest::sha1(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky"), digits = 4),
               "7885f3c09e3fa529cfca3658e214b22fac81f9eb")
  expect_equal(digest::sha1(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000), digits = 4),
               "cb9471e72e3261269799e6193d44c2067a36188d")
  expect_equal(digest::sha1(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit"), digits = 4),
               "d50c261f4a31527fe3ba4281fef66eabb9b9261f")
  expect_equal(digest::sha1(vismodel(flowers, visual = "musca", achromatic = "md.r1", relative = FALSE), digits = 4),
               "07c41c78516ef09dde394d6eec27cefe66c3bc77")
  expect_equal(digest::sha1(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", bkg = "green", vonkries = TRUE, achromatic = "l"), digits = 4),
               "7b348f76891decb04b06ad398cbef1ece1370a4f")
  expect_equal(digest::sha1(vismodel(flowers, visual = "cie10"), digits = 4),
               "ac896df0004fc14da08394ab6ade8f3764e0498a")

  # Attributes
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4),
               "457f2380406720be0fb65d2c7d3efe4efcca5009")
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4),
               "1692dd3afc83d7524d8d971af695f68794c673f2")
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4),
               "81e59dc34d535e29bec068d7fcf7c828d9c3acef")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'musca', achro = 'md.r1', relative = FALSE)), digits = 4),  "3fcd2c3eb74ed4e6d2e505b2c207ca558f287d16")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'apis', relative = FALSE, qcatch = 'Ei', bkg = 'green', vonkries = TRUE, achromatic = 'l')), digits = 4),  "e1dc6128b9c4ce47a0664394f0e453e53ba6c9db")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'cie10')), digits = 4),  "38c06f479375903ba566d9fd7187f9efcf134761")
})

test_that("Warnings", {
  data(flowers)
  data(sicalis)
  fakedat <- as.rspec(data.frame(wl = c(300:700), refl1 = rnorm(401), refl2 = rnorm(401)))

  expect_warning(vismodel(flowers, vonkries = FALSE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = TRUE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = FALSE, achromatic = "l", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, qcatch = "fi", vonkries = TRUE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(fakedat, visual = "bluetit"), "negative")
  expect_warning(vismodel(flowers, qcatch = 'fi', relative = FALSE), "negative")

  test_rspec <- as.rspec(flowers[1:2])
  test_matrix <- as.matrix(flowers[1:2])

  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_rspec), "illum is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_matrix), "illum is a matrix")
  expect_warning(vismodel(flowers, visual = "bluetit", achromatic = test_rspec), "achromatic is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", achromatic = test_matrix), "achromatic is a matrix")
  expect_silent(vismodel(flowers, visual = "bluetit", achromatic = FALSE))
})

test_that("Summary", {
  data(sicalis)
  vis.sicalis <- vismodel(sicalis, visual = "avg.uv")
  summary(vis.sicalis)
})

test_that("Sensmodel", {
  expect_equivalent(colSums(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "Y", "R"))[, -1]), c(1, 1, 1))
  expect_equivalent(round(sum(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "T", "P"), beta = FALSE, integrate = FALSE, om = "bird")[, -1]), 4), 68.271)

  # Errors
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400), "must be included")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400, Bmid = 450), "length")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = "t"), "length")
  expect_error(sensmodel(c(300, 400, 500), Bmid = c(350, 450, 550)), "provided together")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), Bmid = c(350, 450, 550), oiltype = "t"), "only 2")
})
