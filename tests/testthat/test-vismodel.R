context("vismodel")

test_that("Output regression", {
  library(digest)
  data(flowers)

  # Output
  expect_equal(digest::sha1(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky"), digits = 4), "54b3b040113eb655f93be8d9f26eb20d6019cc9b")
  expect_equal(digest::sha1(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000), digits = 4), "d57a50aaf84b7debcb7937d99961280067ce7ab3")
  expect_equal(digest::sha1(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit"), digits = 4), "802350160486ca224f16dd1b5dde0afa5caa893b")
  expect_equal(digest::sha1(vismodel(flowers, visual = "musca", achro = "md.r1", relative = FALSE), digits = 4), "a3ef5be279eadffeb56de61162c632d28ce52fc8")
  expect_equal(digest::sha1(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", bkg = "green", vonkries = TRUE, achromatic = "l"), digits = 4), "896951a2ca1425a09c4907e964cfdf04856185d1")
  expect_equal(digest::sha1(vismodel(flowers, visual = "cie10"), digits = 4), "89689372836a478440853c441f1a87c4caac544b")

  # Attributes
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4), "6d34450fb15cf66e32f95e4e2e803df917e5d46e")
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4), "18df75d8ede8466566cfaf1c444ac0796928867a")
  expect_equal(digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4), "00f45815ec340e8f9271339106e501a65e3e8c62")
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
