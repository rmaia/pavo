imgfake <- as.rimg(matrix(
  c(rep(1, 12), rep(2, 13)), nrow = 10, ncol = 10)
)
polyfake <- data.frame(
  x = c(3, 5, 7, 7, 3),
  y = c(3, 3, 5, 7, 3)
)

test_that("polymask() outside and inside reproduce original image", {

  o <- polymask(
    imgfake,
    polyfake,
    "outside",
    replacement_value = 0
  )

  i <- polymask(
    imgfake,
    polyfake,
    "inside",
    replacement_value = 0
  )

  expect_identical(o + i, imgfake)

})
