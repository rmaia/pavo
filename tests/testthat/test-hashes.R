context("hashes")

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
    "0e01892cc85af65956927dde880a432cb8ef58f3"
  ) # dispace
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "apis", achromatic = "l")), digits = 4),
    "68e113dd2b4ad1bfb80d236fa64f3b5c97e2b48d"
  ) # trispace
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc")), digits = 4),
    "57cb207e9637250020dabdd0d96570a63025dd36"
  ) # tcs
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "musca", achro = "md.r1"), space = "categorical"), digits = 4),
    "681486ec527c0f6e50e6dde1e23831f6c407895e"
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
    "2b51da258f4c5bcaf3a8a851e4e13cbd011c400f"
  ) # hexagon
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "ciexyz"), digits = 4),
    "4738ecfa2f5859134d0578d84bdd103ad7912983"
  ) # ciexyz
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "cielab"), digits = 4),
    "dfc481f4410e335fd63112db92712e4857f6515e"
  ) # cielab
  expect_equal(
    digest::sha1(colspace(vismodel(flowers, visual = "cie10"), space = "cielch"), digits = 4),
    "f4e4cc8da4fdffddc80c51f2f830b88adba3779d"
  ) # cielch

  # sha1() has no method for the 'table' class
  # expect_equal(
  #   digest::sha1(summary(colspace(vismodel(flowers, visual = "cie10"), space = "cielch")), digits = 4),
  #   "8d9c05ec7ae28b219c4c56edbce6a721bd68af82"
  # )
  expect_equivalent(round(sum(summary(colspace(vismodel(flowers)))), 5), 4.08984)
  expect_equivalent(round(sum(summary(colspace(vismodel(flowers))), by = 3), 5), 7.08984)
})

test_that("processing & general", {
  skip_on_cran()

  library(digest)

  # Sensdata
  expect_known_hash(
    expect_silent(sensdata(illum = "all", bkg = "all", trans = "all")),
    "b845b697e6"
  )

  # Peakshape
  expect_known_hash(
    expect_silent(peakshape(flowers, absolute.min = TRUE)),
    "7ac18400b5"
  )

  # Merge
  data(teal)
  teal1 <- teal[, c(1, 3:5)]
  teal2 <- teal[, c(1, 2, 6:12)]
  expect_known_hash(
    expect_silent(merge(teal1, teal2, by = "wl")),
    "235f1a3015"
  )

  # Subset
  data(sicalis)
  vis.sicalis <- vismodel(sicalis)
  tcs.sicalis <- colspace(vis.sicalis, space = "tcs")
  expect_equal(
    digest::sha1(subset(vis.sicalis, "C"), digits = 4),
    "d265007e21606280699956c9df106edbababaa76"
  )
  expect_equal(
    digest::sha1(subset(sicalis, "T", invert = TRUE), digits = 4),
    "332a97ed1c25045b70d871a8686e268d09cefd76"
  )

  # Summary
  expect_known_hash(
    expect_silent(summary(teal)),
    "08ffb15d92"
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
    "b133bda5cc2567ff80a35bdb6d1e5b89e87af8a5"
  )
  expect_equal(
    digest::sha1(summary(snakes), digits = 4),
    "001fb04576b913633f04ea890edef83b41f07e16"
  )
})

test_that("vismodel", {
  skip_on_cran()

  library(digest)
  data(flowers)

  # Output
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky"), digits = 4),
    "d1d8229f54a64ba9d292284e784d196ee6a8021b"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000), digits = 4),
    "fc77a98dd6335db3f5ed24bbda148aa39d0bc4f9"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit"), digits = 4),
    "95275ae220707de802e181798f74f05280a22b93"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "musca", achromatic = "md.r1", relative = FALSE), digits = 4),
    "9a1b514313bdfc4bb24c87b1098971e401c7dd96"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "apis", relative = FALSE, qcatch = "Ei", bkg = "green", vonkries = TRUE, achromatic = "l"), digits = 4),
    "a6bc51f272c930a4ac9e69a1851eca16f5a3a1a0"
  )
  expect_equal(
    digest::sha1(vismodel(flowers, visual = "cie10"), digits = 4),
    "04f0a831e1cf7a50137a0f27df2e36d4c899ae41"
  )

  # Attributes
  if (getRversion() < "4.0.0") {
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4),
      "2f7964c26c7801917bb610c12176664f186a0058"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4),
      "9a9b662b1dc4c81584d45843905c4daf43355c79"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4),
      "4bf4f3392cf0c740a3ab3cfe6a58167346a67ceb"
    )
  } else {
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "canis", achromatic = "all", illum = "bluesky")), digits = 4),
      "f48a7fc9993e7ae4b9607356670db2641c88784f"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "apis", qcatch = "fi", achromatic = "ml", scale = 10000)), digits = 4),
      "0fed595d308c8d09234b35b75bd6569f70de9fad"
    )
    expect_equal(
      digest::sha1(attributes(vismodel(flowers, visual = "bluetit", achromatic = "ch.dc", trans = "bluetit")), digits = 4),
      "89b35af79facc35e7b2ba1c1992c9990ba28383c"
    )
  }
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'musca', achro = 'md.r1', relative = FALSE)), digits = 4),  "3fcd2c3eb74ed4e6d2e505b2c207ca558f287d16")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'apis', relative = FALSE, qcatch = 'Ei', bkg = 'green', vonkries = TRUE, achromatic = 'l')), digits = 4),  "e1dc6128b9c4ce47a0664394f0e453e53ba6c9db")
  # expect_equal(digest::sha1(attributes(vismodel(flowers, visual = 'cie10')), digits = 4),  "38c06f479375903ba566d9fd7187f9efcf134761")
})
