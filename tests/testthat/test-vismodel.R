context("vismodel")

test_that("Warnings", {
  data(flowers)
  data(sicalis)
  fakedat <- as.rspec(data.frame(wl = c(300:700), refl1 = rnorm(401), refl2 = rnorm(401)))

  expect_warning(vismodel(flowers, vonkries = FALSE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = TRUE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = FALSE, achromatic = "l", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, qcatch = "fi", vonkries = TRUE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(fakedat, visual = "bluetit"), "negative")
  expect_warning(vismodel(flowers, qcatch = "fi", relative = FALSE), "negative")

  test_rspec <- as.rspec(flowers[1:2])
  test_matrix <- as.matrix(flowers[1:2])

  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_rspec), "illum is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", illum = test_matrix), "illum is a matrix")
  expect_warning(vismodel(flowers, visual = "bluetit", achromatic = test_rspec), "achromatic is an rspec")
  expect_warning(vismodel(flowers, visual = "bluetit", achromatic = test_matrix), "achromatic is a matrix")
  expect_silent(vismodel(flowers, visual = "bluetit", achromatic = FALSE))
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
