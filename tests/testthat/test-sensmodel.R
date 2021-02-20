test_that("sensmodel() values", {

  expect_equal(colSums(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "Y", "R"))[, -1]), c(1, 1, 1), ignore_attr = TRUE)
  expect_equal(round(sum(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = c("C", "T", "P"), beta = FALSE, integrate = FALSE, om = "bird")[, -1]), 4), 68.271, ignore_attr = TRUE)

  # Custom names
  expect_equal(names(sensmodel(c(300, 400, 500), sensnames = c("s", "m", "l"))), c("wl", "s", "m", "l"))
  expect_equal(names(sensmodel(c(300, 400, 500), sensnames = c("s", "m"))), c("wl", "lmax300", "lmax400", "lmax500"))
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
