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

test_that("sensmodel() templates", {
  # Alpha bands peak where they were asked to, for every template
  for (tmpl in c("govardovskii_a1", "ssh_a1", "ssh_a2")) {
    s <- sensmodel(c(400, 500, 600), template = tmpl, beta = FALSE, integrate = FALSE)
    peaks <- s$wl[apply(s[-1], 2, which.max)]
    expect_equal(peaks, c(400, 500, 600), info = tmpl)
  }

  # Default is unchanged, and is the Govardovskii A1 template
  expect_equal(
    sensmodel(c(400, 500)),
    sensmodel(c(400, 500), template = "govardovskii_a1")
  )
  # 600 nm rather than 400 nm for the SSH templates: 400 nm sits inside the
  # window where the fixed beta band moves the peak, and these are checking the
  # attribute rather than that warning
  expect_identical(attr(sensmodel(400), "template"), "govardovskii_a1")
  expect_identical(attr(sensmodel(600, template = "ssh_a2"), "template"), "ssh_a2")

  # Unambiguous abbreviations resolve, as match.arg intends, but one that could
  # mean either SSH chromophore is rejected rather than silently picking A1
  expect_identical(attr(sensmodel(400, template = "govardovskii"), "template"), "govardovskii_a1")
  expect_identical(attr(sensmodel(600, template = "ssh_a1"), "template"), "ssh_a1")
  expect_error(sensmodel(400, template = "ssh"), "should be one of")

  # A2 is not A1 shifted: at matched peaksens the band is markedly broader.
  # Stavenga et al. (1993) Table 1 gives a0 = 263 for A2 against 380 for A1.
  hbw <- function(x) {
    above <- x$wl[x[[2]] >= 0.5]
    max(above) - min(above)
  }
  for (lmax in c(500, 550, 600)) {
    ratio <- hbw(sensmodel(lmax, template = "ssh_a2", beta = FALSE, integrate = FALSE)) /
      hbw(sensmodel(lmax, template = "ssh_a1", beta = FALSE, integrate = FALSE))
    expect_gt(ratio, 1.15)
    expect_lt(ratio, 1.25)
  }

  # Stavenga (2010) reports the Govardovskii and SSH alpha bands to be close for
  # peaksens above 400 nm, and to diverge in the ultraviolet
  dev <- function(lmax) {
    a <- sensmodel(lmax, template = "govardovskii_a1", beta = FALSE, integrate = FALSE)
    b <- sensmodel(lmax, template = "ssh_a1", beta = FALSE, integrate = FALSE)
    max(abs(a[[2]] - b[[2]]))
  }
  expect_lt(dev(500), 0.05)
  expect_lt(dev(600), 0.05)
  expect_gt(dev(360), 0.10)
})

test_that("sensmodel() SSH beta band shifts the peak of short-wavelength pigments", {
  # The SSH beta band sits at a fixed wavelength rather than scaling with
  # peaksens, so summing it with the alpha band moves the maximum of the result.
  # This is a property of the published template, not an error, which is why
  # beta = FALSE is the documented recommendation rather than a warning. The test
  # exists so that anyone changing the beta band notices they have done so.
  realised <- function(...) {
    s <- sensmodel(..., integrate = FALSE)
    s$wl[which.max(s[[2]])]
  }

  expect_lt(realised(430, template = "ssh_a2"), 430 - 5)
  expect_lt(realised(395, template = "ssh_a1"), 395 - 5)

  # The alpha band alone recovers peaksens exactly, and the default template is
  # unaffected at the same peak sensitivities
  expect_identical(realised(430, template = "ssh_a2", beta = FALSE), 430)
  expect_identical(realised(395, template = "ssh_a1", beta = FALSE), 395)
  expect_identical(realised(430, template = "govardovskii_a1"), 430)

  # Long-wavelength pigments, where the fixed beta band is far from the alpha
  # band, are unaffected
  expect_identical(realised(600, template = "ssh_a2"), 600)

  # None of this warns
  expect_silent(sensmodel(430, template = "ssh_a2"))
  expect_silent(sensmodel(395, template = "ssh_a1"))
})

test_that("sensmodel() errors", {
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400), "must be included")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = 400, Bmid = 450), "length")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), oiltype = "t"), "length")
  expect_error(sensmodel(c(300, 400, 500), Bmid = c(350, 450, 550)), "provided together")
  expect_error(sensmodel(c(300, 400, 500), lambdacut = c(350, 450, 550), Bmid = c(350, 450, 550), oiltype = "t"), "only 2")
})
