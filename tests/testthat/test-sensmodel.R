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

test_that("sensmodel() errors", {
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400), "must be included")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400, Bmid = 450), "length")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = "t"), "length")
  expect_error(sensmodel(c(300, 400, 500), Bmid = c(350, 450, 550)), "provided together")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), Bmid = c(350, 450, 550), oiltype = "t"), "only 2")
})
