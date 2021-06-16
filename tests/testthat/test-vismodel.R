test_that("Warnings", {
  data(flowers)
  data(sicalis)
  fakedat <- as.rspec(data.frame(wl = c(300:700), refl1 = rnorm(401), refl2 = rnorm(401)))

  expect_s3_class(vismodel(flowers), "vismodel")

  expect_warning(vismodel(flowers, vonkries = FALSE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = TRUE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, vonkries = TRUE, relative = FALSE, achromatic = "l", visual = "cie10"), "overriding")
  expect_warning(vismodel(flowers, qcatch = "fi", vonkries = TRUE, relative = FALSE, achromatic = "none", visual = "cie10"), "overriding")
  expect_warning(vismodel(fakedat, visual = "bluetit"), "negative")
  expect_warning(vismodel(flowers, qcatch = "fi", relative = FALSE), "negative")

  test_rspec <- as.rspec(flowers[1:2])
  test_matrix <- as.matrix(flowers[, 2, drop = FALSE])

  expect_warning(vm_rspec <- vismodel(flowers, visual = "bluetit", illum = test_rspec), "illum is an rspec")
  expect_warning(vm_matrix <- vismodel(flowers, visual = "bluetit", illum = test_matrix), "illum is a matrix")

  expect_identical(
    vm_rspec,
    vm_matrix
  )

  expect_warning(vm_rspec <- vismodel(flowers, visual = "bluetit", achromatic = test_rspec), "achromatic is an rspec")
  expect_warning(vm_matrix <- vismodel(flowers, visual = "bluetit", achromatic = test_matrix), "achromatic is a matrix")

  expect_identical(
    vm_rspec,
    vm_matrix
  )

  expect_silent(vismodel(flowers, visual = "bluetit", achromatic = FALSE))

  expect_error(vismodel(flowers, vonkries = TRUE, bkg = NULL), "background is NULL")
  expect_error(vismodel(flowers, trans = NULL), "transmission is NULL")

  expect_warning(flowers_NIR <- as.rspec(flowers, lim = c(300, 1200)), "Interpolating beyond")

  expect_error(vismodel(flowers_NIR), "wavelength range")
  expect_error(vismodel(flowers_NIR, sensmodel(c(350, 450, 550, 650))), "wavelength range")

  expect_silent(vismodel(flowers_NIR, sensmodel(c(350, 450, 550, 650), range = c(300, 1200))))
})

test_that("sensdata()", {
  vis_all <- sensdata(
    visual = "all", achromatic = "all",
    illum = "all", trans = "all",
    bkg = "all"
  )

  # No negative values, no NA
  expect_false(any(vis_all < 0))
  expect_false(anyNA(vis_all))

})
