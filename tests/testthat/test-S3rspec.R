context("rspec")

# File to test S3 functions related to the rspec class

test_that("as.rspec", {
  data(flowers)

  flowers2 <- as.rspec(as.data.frame(flowers))
  expect_s3_class(flowers2, "rspec")

  # Both text and numerically identify wavelength column
  refl1 <- rnorm(401)
  fake1 <- data.frame(wave = 300:700, refl1)
  fake2 <- data.frame(refl1, wave = 300:700)
  expect_equal(as.rspec(fake1, whichwl = "wave"), as.rspec(fake2, whichwl = 2))

  # Interpolation should not happen outside of wl range by default
  flowers3 <- flowers[-1, ]
  expect_equivalent(flowers3, as.rspec(flowers3))

  # With rule = 2, missing values outside of range are generated
  expect_equal(nrow(flowers3) + 1, nrow(as.rspec(flowers3, lim = c(300, 700), exceed.range = TRUE)))
  expect_warning(as.rspec(flowers3, lim = c(300, 700), exceed.range = TRUE), "beyond the range")

  expect_error(as.rspec(c(300:700)), "must be a data frame or matrix")

  colnames(flowers)[2] <- "wl"
  expect_warning(as.rspec(flowers), "Multiple columns named 'wl'")

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
  expect_warning(summary(sicalis, wlmax = 600), "wavelength range not between")
  expect_error(summary(sicalis, wlmin = 200), "wlmin is smaller")
  expect_error(summary(sicalis, wlmax = 1000), "wlmax is larger")

  # Test one spectrum rspec object
  one_spec <- sicalis[, c(1, 2)]
  expect_equal(dim(summary(one_spec)), c(1, 23))
  expect_length(summary(one_spec, wlmin = 500), 23)

  # Error if subset vars do not exist
  expect_error(summary(sicalis, subset = "H9"), "do not match color variable names")

  # Warning about UV variables if full UV range is not included
  expect_warning(summary(sicalis, wlmin = 350), "UV-related variables may not be meaningful")
})

test_that("plot.rspec", {

  data(teal)

  expect_message(plot(teal, type = "heatmap"), "No varying vector supplied")

})

test_that("subset.rspec", {

  data(sicalis)

  sicalis_T <- subset(sicalis, "T")

  expect_equal(dim(sicalis_T), c(401, 8))
  expect_is(sicalis_T, "rspec")

  expect_warning(subset(sicalis, "Z"), "Subset condition not found")

})

test_that("merge.rspec", {

  data(sicalis)

  sicalis_T <- subset(sicalis, "T")
  sicalis_C <- subset(sicalis, "C")

  sicalis_TC <- subset(sicalis, c("T", "C"))

  sicalis_merged <- merge(sicalis_T, sicalis_C)

  expect_is(sicalis_merged, "rspec")
  expect_mapequal(sicalis_TC, sicalis_merged)

  expect_error(merge(sicalis_T, as.data.frame(sicalis_C)),
               "invalid rspec objects")

  expect_error(merge(sicalis_T, sicalis_C[, -1]),
               "Cannot find valid")

})
