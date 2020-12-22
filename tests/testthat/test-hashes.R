test_that("coldist", {
  skip_on_cran()

  data(flowers)
  library(digest)

  # JND transform
  vis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)
  # expect_equal(digest::sha1(jndrot(jnd2xyz(coldist(vismodel(flowers, achromatic = "bt.dc", relative = FALSE), achromatic = TRUE))), digits = 4),
  #              "07064d68561bad24d8f02c0413611b5ba49ec53a")

  # Output
  expect_equal(
    digest::sha1(coldist(colspace(vismodel(flowers, visual = "canis", achromatic = "ml")), achromatic = TRUE), digits = 4),
    "bc460149b2263a857c9d573e77169556fa641f56"
  )
  # expect_equal(digest::sha1(coldist(vismodel(flowers, visual = 'canis', achromatic = 'ml'), achromatic = TRUE, n = c(1, 1)), digits = 4),
  #              "7329a3c550fe1d2939423e4104066c868891914f")
  expect_equal(
    digest::sha1(coldist(colspace(vismodel(flowers, visual = "canis", achromatic = "all")), n = c(1, 2), achromatic = TRUE, subset = "Hibbertia_acicularis"), digits = 4),
    "27ab9af8efe2b1651cd36f8506262f87e2b127a7"
  )
  expect_equal(
    digest::sha1(coldist(colspace(vismodel(flowers, visual = "apis", achromatic = "all", relative = FALSE, vonkries = TRUE), space = "hexagon"), n = c(1, 2), achromatic = TRUE, subset = c("Hibbertia_acicularis", "Grevillea_buxifolia")), digits = 4),
    "754c01809100bdacc80d40db2359797f41180c23"
  )
  expect_equal(
    digest::sha1(coldist(colspace(vismodel(flowers, visual = "segment")), achromatic = TRUE), digits = 4),
    "d65c018342664ae9c8dca35e715c57dde28de30a"
  )
  expect_equal(
    digest::sha1(coldist(as.matrix(vismodel(flowers, achro = "bt.dc")), qcatch = "Qi", achromatic = TRUE), digits = 4),
    "c6d1989e98abd7772c00475696c6e6dafe0a2e46"
  )
})

test_that("colspace", {
  skip_on_cran()

  library(digest)
  data(flowers)

  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "canis", achromatic = "all")), digits = 4),
    "47352906f00a35504177712d772737b33a6ede64"
  ) # dispace
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "apis", achromatic = "l")), digits = 4),
    "81aa34c6d3f86e5644f2d2793d03bd14cb7281da"
  ) # trispace
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc")), digits = 4),
    "56b236d6f0591a25b067009a10dc2d96c3e27c4f"
  ) # tcs
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "musca", achro = "md.r1"), space = "categorical"), digits = 4),
    "b20853b3e52a60f2dd17b418a48b681d7f49e7d1"
  ) # categorical
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "segment", achromatic = "bt.dc"), space = "segment"), digits = 4),
    "f47081fbc5f3f896fc50b2223937d91b6f61069e"
  ) # segment
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", vonkries = TRUE, achromatic = "l"), space = "coc"), digits = 4),
    "d6e5c22dd45d2604c0d2fc16509b8887cb7812d2"
  ) # coc
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "apis", qcatch = "Ei", vonkries = TRUE, relative = FALSE, achromatic = "l"), space = "hexagon"), digits = 4),
    "a1fdd24e315413825c94d4caf1164b8be57c8156"
  ) # hexagon
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "ciexyz"), digits = 4),
    "99684793a0db286562bff697354496ac3ef0abfb"
  ) # ciexyz
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "cielab"), digits = 4),
    "55961f7e22403fba0c0c658868918722befb5f2c"
  ) # cielab
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "cielch"), digits = 4),
    "e0ad250b695e97c9ffb53c0303f19d908e33d033"
  ) # cielch

  # sha1() has no method for the 'table' class
  # expect_equal(
  #   digest::sha1(summary(colspace(vismodel(flowers, visual = "cie10"), space = "cielch")), digits = 4),
  #   "8d9c05ec7ae28b219c4c56edbce6a721bd68af82"
  # )
})

test_that("voloverlap()", {
  skip_on_cran()

  data(sicalis)
  tcs.sicalis.C <- subset(colspace(vismodel(sicalis)), "C")
  tcs.sicalis.T <- subset(colspace(vismodel(sicalis)), "T")
  tcs.sicalis.B <- subset(colspace(vismodel(sicalis)), "B")

  expect_equal(
    digest::sha1(voloverlap(tcs.sicalis.T, tcs.sicalis.B, type = "convex"), digits = 4),
    "3717422024683f1e3e1bd8dbfe832b177147afce"
  )

  expect_equal(
    digest::sha1(voloverlap(tcs.sicalis.T, tcs.sicalis.C, type = "convex"), digits = 4),
    "69b323778e83f2e43a91d60326f1e726eb2cd0e4"
  )
})

test_that("processing & general", {
  skip_on_cran()

  library(digest)

  # Sensdata
  expect_known_hash(
    expect_silent(sensdata(illum = "all", bkg = "all", trans = "all")),
    "b084b37ec7"
  )

  # Peakshape
  expect_known_hash(
    expect_silent(peakshape(flowers, absolute.min = TRUE)),
    "7fbaba1738"
  )

  # Merge
  data(teal)
  teal1 <- teal[, c(1, 3:5)]
  teal2 <- teal[, c(1, 2, 6:12)]
  expect_known_hash(
    expect_silent(merge(teal1, teal2, by = "wl")),
    "02df3eedf3"
  )

  # Subset
  data(sicalis)
  vis.sicalis <- vismodel(sicalis)
  tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
  expect_equal(
    digest::sha1(subset(vis.sicalis, "C"), digits = 4),
    "93ae671c250d2d4f0f5dcf9e714eb497d8baf74f"
  )
  expect_equal(
    digest::sha1(subset(sicalis, "T", invert = TRUE), digits = 4),
    "332a97ed1c25045b70d871a8686e268d09cefd76"
  )

  # Summary
  expect_known_hash(
    expect_silent(summary(teal)),
    "c64e1fd403"
  )
  expect_known_hash(
    expect_silent(summary(sicalis)),
    "66129550f3"
  )
})

test_that("images", {
  skip_on_cran()

  library(digest)
  suppressWarnings(RNGversion("3.5.0")) # back compatibility for now
  set.seed(2231)

  papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
  snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))

  expect_equal(
    digest::sha1(summary(papilio), digits = 4),
    "aa1c46d4796c523f51c4e959ac90a692dd8ecfe4"
  )
  expect_equal(
    digest::sha1(summary(snakes), digits = 4),
    "dd7fc9fd7c41da84c641181a9a3701da74f3c41e"
  )
})

test_that("vismodel", {
  skip_on_cran()

  library(digest)
  data(flowers)

  # Output
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky"), digits = 4),
    "61879badc0cb518ebd8f62f9c8838c7b32cb51ff"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000), digits = 4),
    "4a3539c87d1c672510df68992b9dc6954337a736"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit"), digits = 4),
    "fbd9f6b5368f2c81f11ec86a78322e7a14cc7f47"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "musca", achromatic = "md.r1", relative = FALSE), digits = 4),
    "edcb721c2093c7af40efdae94837c4e01031c8ae"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", bkg = "green", vonkries = TRUE, achromatic = "l"), digits = 4),
    "a6bc51f272c930a4ac9e69a1851eca16f5a3a1a0"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "cie10"), digits = 4),
    "fc5f5f2f11fefdcff1bbdd264e28d520f0812712"
  )

  # Attributes
  if (getRversion() < "4.0.0") {
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4),
      "0f788526e4db68c9921e441066779146f8a4c377"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4),
      "b9d488a8e36bca04a66e4513e781c21b66c10ce9"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4),
      "0a3fb5b639d4c4224cf91045d5f8a13cc06f8550"
    )
  } else {
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4),
      "397eda31c6948884e09ef58a1b8f0ec5d4f3401c"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4),
      "60744da46c20782fa8af0bf2a93316ed0d2e6e9d"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4),
      "7bca56baefdd2a42fcb59c26614fe0bbec326865"
    )
  }
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'musca', achro = 'md.r1', relative = FALSE)), digits = 4),  "3fcd2c3eb74ed4e6d2e505b2c207ca558f287d16")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'apis', relative = FALSE, qcatch = 'Ei', bkg = 'green', vonkries = TRUE, achromatic = 'l')), digits = 4),  "e1dc6128b9c4ce47a0664394f0e453e53ba6c9db")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'cie10')), digits = 4),  "38c06f479375903ba566d9fd7187f9efcf134761")
})
