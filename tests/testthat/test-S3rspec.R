library(pavo)
context("rspec")

# File to test S3 functions related to the rspec class

test_that("as.rspec", {
  data(flowers)

  flowers2 <- as.rspec(as.data.frame(flowers))
  expect_s3_class(flowers2, "rspec")
  
  # Both text and numerically identify wavelength column
  refl1 = rnorm(401)
  fake1 <- data.frame(wave = 300:700, refl1)
  fake2 <- data.frame(refl1, wave = 300:700)
  expect_equal(as.rspec(fake1, whichwl = 'wave'), as.rspec(fake2, whichwl = 2))
  
})

test_that("summary.rspec", {
  data(sicalis)

  expect_equal(dim(summary(sicalis)), c(21, 23))
  # expect_known_hash(summary(sicalis), "66129550f3")

  # Subset
  expect_named(summary(sicalis, subset = TRUE), c("B2", "S8", "H1"))
  expect_named(summary(sicalis, subset = c("B1", "H4")), c("B1", "H4"))

  # Different wl ranges
  expect_warning(summary(sicalis, wlmin = 500), "wavelength range not between")
  expect_warning(summary(sicalis[1:200, ]), "wavelength range not between")
  expect_error(summary(sicalis, wlmax = 1000), "wlmax is larger")

  # Test one spectrum rspec object
  one_spec <- sicalis[, c(1, 2)]
  expect_equal(dim(summary(one_spec)), c(1, 23))
})
