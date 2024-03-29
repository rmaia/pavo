# File to test S3 functions related to the rspec class

test_that("as.rspec", {
  data(flowers)
  expect_true(is.rspec(flowers))

  expect_message(
    flowers2 <- as.rspec(as.data.frame(flowers)),
    "wavelengths found in column 1"
  )
  expect_s3_class(flowers2, "rspec")
  expect_identical(flowers, flowers2)

  # Both text and numerically identify wavelength column
  refl1 <- rnorm(401, mean = 5)
  fake1 <- data.frame(wave = 300:700, refl1)
  fake2 <- data.frame(refl1, wave = 300:700)
  expect_identical(
    as.rspec(fake1, whichwl = "wave"),
    as.rspec(fake2, whichwl = 2)
  )

  # Interpolation should not happen outside of wl range by default
  flowers3 <- flowers[-1, ]
  rownames(flowers3) <- NULL
  expect_message(converted_flowers <- as.rspec(flowers3))
  expect_identical(
    flowers3,
    converted_flowers
  )

  # With rule = 2, missing values outside of range are generated
  expect_warning(
    flowers3_fullrange <- as.rspec(flowers3, lim = c(300, 700), exceed.range = TRUE),
    "beyond the range"
  )
  expect_identical(dim(flowers3_fullrange), c(401L, 37L))

  expect_error(as.rspec(300:700), "must be a data frame or matrix")

  colnames(flowers)[2] <- "wl"
  expect_warning(as.rspec(flowers), "Multiple columns named 'wl'")

  fakedat <- data.frame(wl = seq(200, 800, 0.5), refl1 = rnorm(1201), refl2 = rnorm(1201))
  expect_identical(dim(as.rspec(fakedat, lim = c(300, 700), interp = FALSE)), c(801L, 3L))
  expect_identical(dim(as.rspec(fakedat, lim = c(200, 800), interp = FALSE)), c(1201L, 3L))
  expect_identical(dim(as.rspec(fakedat, lim = c(300, 700), interp = TRUE)), c(401L, 3L))
  expect_identical(as.data.frame(as.rspec(fakedat, lim = c(300, 700))["wl"]), data.frame(wl = 300L:700L))

  # Matrix and df input should have the same output
  expect_identical(as.rspec(fakedat), as.rspec(as.matrix(fakedat)))

  # Single column df should work and output a 2 columns rspec object
  expect_identical(dim(suppressWarnings(as.rspec(fakedat[, 2, drop = FALSE]))), c(1201L, 2L))

  expect_message(as.rspec(fakedat), "wavelengths found")

  expect_warning(as.rspec(fakedat[, -1], lim = c(300, 700)), "user-specified range")
  expect_warning(as.rspec(fakedat[, -1]), "arbitrary index")

  expect_identical(
    as.rspec(fakedat, lim = c(300.1, 700), interp = FALSE)$wl,
    seq(300.5, 700, 0.5)
  )
  expect_identical(
    as.rspec(fakedat, lim = c(300, 699.1), interp = FALSE)$wl,
    seq(300, 699, 0.5)
  )

  fakedat[2:4, 2] <- NA
  expect_message(as.rspec(fakedat, whichwl = 1), "negative")
  expect_message(as.rspec(fakedat, whichwl = 1), "NA")

  flowers[, 2] <- as.character(flowers[, 2])
  expect_error(as.rspec(flowers), "numeric")
})

test_that("summary.rspec", {
  data(sicalis)

  expect_identical(dim(summary(sicalis)), c(21L, 23L))
  # expect_known_hash(summary(sicalis), "66129550f3")

  # Subset
  expect_named(summary(sicalis, subset = TRUE), c("B2", "S8", "H1"))
  expect_named(summary(sicalis, subset = c("B1", "H4")), c("B1", "H4"))

  # Different wl ranges
  expect_warning(summary(sicalis, lim = c(500, 700)), "wavelength range not between")
  expect_warning(summary(sicalis[1:200, ]), "wavelength range not between")
  expect_warning(summary(sicalis, lim = c(300, 600)), "wavelength range not between")
  expect_error(summary(sicalis, lim = c(200, 700)), "smaller than the range")
  expect_error(summary(sicalis, lim = c(300, 1000)), "larger than the range")

  # Test one spectrum rspec object
  one_spec <- sicalis[, c(1, 2)]
  expect_identical(dim(summary(one_spec)), c(1L, 23L))
  expect_warning(
    expect_length(summary(one_spec, lim = c(500, 700)), 23), "blue chroma"
  )

  # Error if subset vars do not exist
  expect_error(summary(sicalis, subset = "H9"), "do not match color variable names")

  # Warning about UV variables if full UV range is not included
  expect_warning(summary(sicalis, lim = c(350, 700)), "UV-related variables may not be meaningful")
})

test_that("plot.rspec", {
  data(teal)

  expect_message(plot(teal, type = "heatmap"), "No varying vector supplied")
})

test_that("subset.rspec", {
  data(sicalis)

  sicalis_T <- subset(sicalis, "T")

  expect_identical(dim(sicalis_T), c(401L, 8L))
  expect_s3_class(sicalis_T, "rspec")

  expect_warning(subset(sicalis, "Z"), "Subset condition not found")
})

test_that("merge.rspec", {
  data(sicalis)

  sicalis_T <- subset(sicalis, "T")
  sicalis_C <- subset(sicalis, "C")

  sicalis_TC <- subset(sicalis, c("T", "C"))

  sicalis_merged <- merge(sicalis_T, sicalis_C)

  expect_s3_class(sicalis_merged, "rspec")
  expect_mapequal(sicalis_TC, sicalis_merged)

  expect_error(
    merge(sicalis_T, as.data.frame(sicalis_C)),
    "invalid rspec objects"
  )

  expect_error(
    merge(sicalis_T, sicalis_C[, -1]),
    "Cannot find valid"
  )
})
