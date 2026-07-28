test_that("sensmodel() values", {
  expect_snapshot_value(
    sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "Y", "R")),
    style = "json2", tolerance = 1e-5
  )

  expect_snapshot_value(
    sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "T", "P"), beta = FALSE, integrate = FALSE, om = "bird"),
    style = "json2", tolerance = 5e-5
  )

  expect_snapshot_value(
    # Danio aequipinnatus based on Govardovskii et al. (2000)
    sensmodel(c(357, 411, 477, 569)),
    style = "json2", tolerance = 1e-5
  )

  # Custom names
  expect_named(sensmodel(c(300, 400, 500), sensnames = c("s", "m", "l")), c("wl", "s", "m", "l"))
  expect_named(sensmodel(c(300, 400, 500), sensnames = c("s", "m")), c("wl", "lmax300", "lmax400", "lmax500"))
  expect_message(names(sensmodel(c(300, 400, 500), sensnames = c("s", "m"))), "length of argument")

  # S3 class
  daniosens <- sensmodel(c(357, 411, 477, 569))
  expect_s3_class(daniosens, "sensmod")
  expect_s3_class(daniosens, "rspec")
})

test_that("sensmodel() curves do not depend on range", {
  # The alpha-band expression of Govardovskii et al. (2000) contains a constant
  # of 300 nm, which was previously coded as range[1]. The two coincide at the
  # default range, so the shape of a curve silently changed with the requested
  # range. Sensitivities must be identical over a shared window whatever range
  # was used to generate them.

  sens_cols <- function(x, from) unname(as.matrix(x[x$wl >= from, -1, drop = FALSE]))

  # Raising the lower bound
  full <- sensmodel(c(450, 500, 560), range = c(300, 700), integrate = FALSE)
  raised <- sensmodel(c(450, 500, 560), range = c(400, 700), integrate = FALSE)
  expect_equal(sens_cols(full, 400), sens_cols(raised, 400))

  # Lowering it, which affects UV pigments in the other direction
  lowered <- sensmodel(c(360, 450, 560), range = c(250, 700), integrate = FALSE)
  base <- sensmodel(c(360, 450, 560), range = c(300, 700), integrate = FALSE)
  expect_equal(sens_cols(lowered, 300), sens_cols(base, 300))
})

test_that("sensmodel() errors", {
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400), "must be included")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400, Bmid = 450), "length")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = "t"), "length")
  expect_error(sensmodel(c(300, 400, 500), Bmid = c(350, 450, 550)), "provided together")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), Bmid = c(350, 450, 550), oiltype = "t"), "only 2")
})
