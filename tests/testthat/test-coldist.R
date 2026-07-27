test_that("Errors", {
  data(flowers)

  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3, 4, 5), achromatic = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), n = c(1, 2, 3), achromatic = FALSE), "different length")
  expect_error(coldist(vismodel(flowers, relative = FALSE), subset = c("Goodenia", "Xyris", "Eucalyptus"), achromatic = FALSE), "Too many")
  expect_error(coldist(as.matrix(vismodel(flowers)), noise = "quantum"), "quantum receptor noise model")
  expect_error(coldist(vismodel(flowers, qcatch = "Ei")), "not compatible with hyperbolically")
  expect_error(coldist(as.matrix(vismodel(flowers))), "quantum catches not defined")
  expect_error(coldist(vismodel(flowers), weber.ref = 5), "greater than the length of vector")
})

test_that("Messages & warnings", {
  data(flowers)

  expect_message(coldist(vismodel(flowers, relative = FALSE)), "noise-weighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "segment"))), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "musca"), space = "categorical")), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers, visual = "cie2"), space = "cielab")), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = "cie10"), space = "cielch")), "CIE2000 distances")
  expect_message(coldist(colspace(vismodel(flowers, visual = "cie10"), space = "ciexyz")), "unweighted Euclidean")
  expect_message(coldist(colspace(vismodel(flowers,
    visual = "apis", relative = FALSE,
    qcatch = "Ei", vonkries = TRUE
  ), space = "coc")), "Manhattan distances")
  expect_message(coldist(colspace(vismodel(flowers,
    visual = "apis", relative = FALSE,
    qcatch = "Ei", vonkries = TRUE
  ), space = "hexagon")), "unweighted Euclidean")
  expect_message(coldist(as.matrix(vismodel(flowers, achromatic = "bt.dc")), qcatch = "Qi", achromatic = TRUE), "last column ignored for chromatic contrast")
  expect_message(coldist(as.matrix(vismodel(flowers)), qcatch = "Qi"), "Number of cones assumed to be 4")

  expect_message(coldist(vismodel(flowers)), "Quantum catch are relative")
  expect_message(coldist(vismodel(flowers), achromatic = TRUE), "achromatic contrast not calculated")

  expect_error(coldist(vismodel(flowers, relative = FALSE), noise = "quantum"), "negative quantum-catch")
})

test_that("Equivalent", {
  data(flowers)

  expect_identical(
    coldist(vismodel(flowers, relative = FALSE), weber.ref = "longest", achromatic = FALSE),
    coldist(vismodel(flowers, relative = FALSE), weber.ref = 4, achromatic = FALSE)
  )

  expect_identical(
    coldist(vismodel(flowers, relative = FALSE), weber.ref = "longest", achromatic = FALSE),
    coldist(vismodel(flowers, relative = FALSE), weber.ref = 4, achromatic = FALSE)
  )

  expect_equal(
    coldist(vismodel(flowers, relative = TRUE), achromatic = FALSE),
    suppressWarnings(coldist(vismodel(flowers, relative = FALSE), achromatic = FALSE)),
    ignore_attr = TRUE
  )

  expect_equal(
    suppressWarnings(coldist(vismodel(flowers))),
    suppressWarnings(coldist(as.data.frame(vismodel(flowers)), qcatch = "Qi")),
    ignore_attr = TRUE
  )

  expect_equal(
    coldist(colspace(vismodel(flowers, visual = "cie10", vonkries = TRUE, relative = FALSE), "cielab")),
    coldist(colspace(vismodel(flowers, visual = "cie10", vonkries = TRUE, relative = FALSE), "cielch")),
    ignore_attr = TRUE
  )
})

test_that("Options", {
  data(sicalis)

  expect_length(coldist(vismodel(sicalis, achromatic = "bt.dc", qcatch = "fi", illum = 1000, relative = FALSE),
    noise = "quantum",
    achromatic = TRUE
  ), 4)
})

test_that("jnd transform", {
  data(flowers)

  vis.flowers <- vismodel(flowers, visual = "apis")
  cd.flowers <- coldist(vis.flowers, n = c(1, 1, 1))
  jnd.flowers <- jnd2xyz(cd.flowers)

  # Errors
  expect_identical(dim(jnd2xyz(cd.flowers)), c(36L, 2L))
  expect_identical(dim(coldist(vismodel(flowers))), c(630L, 4L))

  rownames(attr(jnd.flowers, "resref"))[4] <- "nope"
  expect_error(jndrot(jnd.flowers), "does not match")

  class(jnd.flowers) <- "data.frame"
  expect_error(jndrot(jnd.flowers), "jnd2xyz")
})

test_that("Output", {
  # Maximum possible unweighted Euclidean distances
  di <- data.frame(s = c(0, 1), l = c(1, 0))
  expect_identical(coldist(colspace(di, space = "di"))[["dS"]], (1 / sqrt(2)) * 2)

  tri <- data.frame(s = c(0, 0), m = c(0, 1), l = c(1, 0))
  expect_identical(coldist(colspace(tri, space = "tri"))[["dS"]], (1 / sqrt(2)) * 2)

  tetra <- data.frame(u = c(0, 1), s = c(0, 0), m = c(0, 0), l = c(1, 0))
  expect_identical(coldist(colspace(tetra, space = "tcs"))[["dS"]], (sqrt(3) / (2 * sqrt(2))) * 2)
})

test_that("bootcoldist", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))

  bcd <- suppressWarnings(
    bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1)
  )
  expect_identical(dim(bcd), c(3L, 6L))

  # Raw size
  raw <- bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 30, raw = TRUE)
  expect_identical(nrow(raw), 30L)
  raw2 <- bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 437, raw = TRUE)
  expect_identical(nrow(raw2), 437L)
})

test_that("bootcoldist averages colspace coordinates arithmetically", {
  data(sicalis)
  space <- colspace(vismodel(sicalis, visual = "apis", achromatic = "l"))
  gr <- gsub("ind..", "", rownames(space))

  bcd <- suppressMessages(bootcoldist(space, by = gr, boot.n = 100))

  # Distances between colspace objects are measured in their coordinates, so a
  # group's centroid is the arithmetic mean of those coordinates and the
  # empirical distances are simply the distances between centroids. Averaging
  # geometrically, as earlier versions did, would not give this
  centroids <- t(vapply(
    split(space[c("x", "y")], gr),
    function(x) colMeans(as.matrix(x)),
    numeric(2)
  ))
  expected <- as.matrix(dist(centroids))

  expect_equal(bcd["B-C", "dS.mean"], expected["B", "C"], tolerance = 1e-10)
  expect_equal(bcd["B-T", "dS.mean"], expected["B", "T"], tolerance = 1e-10)
  expect_equal(bcd["C-T", "dS.mean"], expected["C", "T"], tolerance = 1e-10)
})

test_that("bootcoldist handles spaces with negative coordinates", {
  data(flowers)

  # CIELAB's a and b axes are routinely negative, which a geometric mean cannot
  # summarise, so these spaces previously came back as NaN
  lab <- colspace(
    vismodel(flowers, visual = "cie10", vonkries = TRUE, relative = FALSE),
    space = "cielab"
  )
  gr <- rep(c("a", "b"), each = nrow(lab) / 2)

  res <- suppressMessages(bootcoldist(lab, by = gr, boot.n = 100, achromatic = FALSE))

  expect_identical(dim(res), c(1L, 3L))
  expect_false(anyNA(res))
  expect_gt(res[, "dS.mean"], 0)
  expect_true(all(res[, "dS.lwr"] <= res[, "dS.upr"]))
})

test_that("bootlimits", {
  # A known bootstrap distribution, so the limits can be checked by hand
  bootvals <- matrix(as.numeric(seq_len(1000)), ncol = 1, dimnames = list(NULL, "a-b"))
  probs <- c(0.025, 0.975)

  # Without jackknife values these are plain percentiles: the 25th and 975th
  # of a thousand sorted replicates
  plain <- bootlimits(bootvals, c(`a-b` = 500), NULL, probs)
  expect_identical(unname(plain[, 1]), c(25, 975))
  expect_identical(colnames(plain), "a-b")

  # A symmetric jackknife has no skew to correct for, and an empirical value at
  # the median of the bootstrap distribution has no bias, so BCa should return
  # the percentile limits unchanged
  symmetric <- matrix(c(-2, -1, 0, 1, 2), ncol = 1)
  expect_identical(
    bootlimits(bootvals, c(`a-b` = 500.5), symmetric, probs),
    plain
  )

  # An empirical value sitting below the middle of the bootstrap distribution
  # pulls both limits down, and above it pushes them up
  low <- bootlimits(bootvals, c(`a-b` = 250), symmetric, probs)
  high <- bootlimits(bootvals, c(`a-b` = 750), symmetric, probs)
  expect_true(all(low[, 1] < plain[, 1]))
  expect_true(all(high[, 1] > plain[, 1]))

  # Limits stay inside the bootstrap distribution however extreme the correction
  extreme <- bootlimits(bootvals, c(`a-b` = 1), symmetric, probs)
  expect_gte(min(extreme), 1)
  expect_lte(max(extreme), 1000)
})

test_that("bootcoldist BCa intervals", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))
  ind <- substr(rownames(vm), 1, 4)

  set.seed(1)
  perc <- suppressWarnings(bootcoldist(
    vm,
    by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 200
  ))
  set.seed(1)
  bca <- suppressWarnings(bootcoldist(
    vm,
    by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 200,
    ci.type = "bca"
  ))

  expect_identical(dim(bca), dim(perc))
  expect_identical(dimnames(bca), dimnames(perc))

  # Same resampling, same point estimates, only the limits move
  expect_identical(bca[, "dS.mean"], perc[, "dS.mean"])
  expect_identical(bca[, "dL.mean"], perc[, "dL.mean"])
  limits <- c("dS.lwr", "dS.upr", "dL.lwr", "dL.upr")
  expect_false(isTRUE(all.equal(bca[, limits], perc[, limits])))

  expect_true(all(bca[, "dS.lwr"] <= bca[, "dS.upr"]))
  expect_true(all(bca[, "dL.lwr"] <= bca[, "dL.upr"]))

  # Limits are order statistics of the bootstrap distribution, so they cannot
  # fall outside the range of the distances that were actually resampled
  set.seed(1)
  rawvals <- suppressWarnings(bootcoldist(
    vm,
    by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 200,
    raw = TRUE
  ))
  expect_gte(bca["B-C", "dS.lwr"], min(rawvals[["B-C_dS"]]))
  expect_lte(bca["B-C", "dS.upr"], max(rawvals[["B-C_dS"]]))

  # BCa also works with cluster resampling, jackknifing whole individuals
  set.seed(1)
  clustered <- suppressWarnings(bootcoldist(
    vm,
    by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 200,
    cluster = ind, ci.type = "bca"
  ))
  expect_identical(dim(clustered), dim(bca))
  expect_identical(clustered[, "dS.mean"], bca[, "dS.mean"])

  expect_error(
    bootcoldist(vm, by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, ci.type = "student"),
    "should be one of"
  )
})

test_that("bootcoldist falls back to percentile limits", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")

  # A group of one cannot be jackknifed, since leaving its only row out leaves
  # nothing to take a mean of
  gr <- gsub("ind..", "", rownames(vm))
  gr[1] <- "solo"

  expect_warning(
    bootcoldist(
      vm,
      by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 100,
      ci.type = "bca"
    ),
    "[Ff]alling back to percentile"
  )
})

test_that("bootcoldist requires enough replicates for alpha", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))

  # round(10 * 0.025) is 0, which would quietly drop a row from the interval
  expect_error(
    suppressWarnings(bootcoldist(
      vm,
      by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 10
    )),
    "too small to estimate"
  )

  # 19 is the last replicate count that fails at the default alpha: 20 * 0.025
  # lands a hair above one half in floating point, and so rounds up to 1
  expect_error(
    suppressWarnings(bootcoldist(
      vm,
      by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 19
    )),
    "too small to estimate"
  )
})

test_that("bootcoldist arguments don't capture coldist arguments", {
  # Any formal placed before the dots can swallow an argument meant for
  # coldist() by partial matching. 'n' is a prefix of 'nesting', so keeping the
  # clustering arguments after the dots is what stops n = c(1, 2, 3) from being
  # read as a nesting structure.
  fm <- names(formals(bootcoldist))
  expect_true(all(match(c("cluster", "nesting"), fm) > match("...", fm)))

  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))

  expect_no_error(
    suppressWarnings(bootcoldist(
      vm,
      by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 100
    ))
  )
})

test_that("bootcoldist hierarchical resampling", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))
  ind <- substr(rownames(vm), 1, 4)

  # Clustering changes which rows are drawn, but not the empirical means, so
  # the point estimates must be identical either way
  set.seed(1)
  flat <- suppressWarnings(bootcoldist(
    vm,
    by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))
  set.seed(1)
  clustered <- suppressWarnings(bootcoldist(
    vm,
    by = gr, cluster = ind, n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))

  expect_identical(dim(clustered), c(3L, 6L))
  expect_identical(dimnames(clustered), dimnames(flat))
  expect_identical(clustered[, "dS.mean"], flat[, "dS.mean"])
  expect_identical(clustered[, "dL.mean"], flat[, "dL.mean"])

  # Confidence limits are still bracketing, and the resampling is reproducible
  expect_true(all(clustered[, "dS.lwr"] <= clustered[, "dS.upr"]))
  set.seed(1)
  again <- suppressWarnings(bootcoldist(
    vm,
    by = gr, cluster = ind, n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))
  expect_identical(again, clustered)

  # Clusters can also be supplied as a factor or as numbers
  set.seed(1)
  asfactor <- suppressWarnings(bootcoldist(
    vm,
    by = gr, cluster = factor(ind), n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))
  expect_identical(asfactor, clustered)

  # Nested designs, where each individual belongs to a single group
  grp <- ifelse(ind %in% paste0("ind", 1:3), "g1", "g2")
  nested <- suppressMessages(suppressWarnings(bootcoldist(
    vm,
    by = grp, cluster = ind, n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  )))
  expect_identical(dim(nested), c(1L, 6L))
})

test_that("bootcoldist widens intervals for pseudoreplicated data", {
  # Two populations of ten individuals, each measured five times. Individuals
  # differ in colour, repeated measurements of one individual barely differ at
  # all, so there are ten independent colours per group and not fifty.
  #
  # Note that the variation has to sit in the ratios between cones. Scaling
  # every cone by the same factor is a pure intensity difference, and cancels
  # out of the chromatic distance entirely.
  set.seed(20250725)

  nind <- 10
  nrep <- 5
  base <- c(u = 0.05, s = 0.10, m = 0.15, l = 0.20)

  simulate_group <- function(group, shift) {
    ind <- rep(seq_len(nind), each = nrep)
    colours <- exp(matrix(rnorm(nind * length(base), 0, 0.25), nrow = nind))
    catches <- colours[ind, ] * exp(rnorm(nind * nrep * length(base), 0, 0.01))
    catches <- sweep(catches, 2, base * shift, "*")

    out <- as.data.frame(catches)
    names(out) <- names(base)
    out$by <- group
    out$cluster <- paste0(group, "_ind", ind)
    out
  }

  # a chromatic difference between the groups, not merely a brighter one
  dat <- rbind(
    simulate_group("g1", c(u = 1, s = 1, m = 1, l = 1)),
    simulate_group("g2", c(u = 1, s = 1, m = 1, l = 1.15))
  )
  qcatches <- dat[names(base)]

  naive <- suppressMessages(bootcoldist(
    qcatches,
    by = dat$by, n = c(1, 2, 2, 4), weber = 0.1,
    achromatic = FALSE, qcatch = "Qi", boot.n = 200
  ))
  clustered <- suppressMessages(bootcoldist(
    qcatches,
    by = dat$by, n = c(1, 2, 2, 4), weber = 0.1,
    achromatic = FALSE, qcatch = "Qi", boot.n = 200,
    cluster = dat$cluster
  ))

  expect_identical(clustered[, "dS.mean"], naive[, "dS.mean"])

  width <- function(x) unname(x[, "dS.upr"] - x[, "dS.lwr"])
  expect_gt(width(clustered), width(naive))

  # Five near-identical measurements per individual, so the naive interval is
  # roughly sqrt(5) too narrow. The bound is loose because ten clusters make
  # for a noisy bootstrap
  expect_gt(width(clustered) / width(naive), 1.3)
})

test_that("bootcoldist cluster errors", {
  data(sicalis)
  vm <- vismodel(sicalis, visual = "apis", achromatic = "l")
  gr <- gsub("ind..", "", rownames(vm))
  ind <- substr(rownames(vm), 1, 4)

  expect_error(
    bootcoldist(vm, by = gr, cluster = ind[-1], n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1),
    "one entry per row"
  )
  expect_error(
    bootcoldist(vm, by = gr, cluster = replace(ind, 1, NA), n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1),
    "missing values"
  )
  expect_error(
    bootcoldist(vm, by = gr, cluster = ind, nesting = "sideways", n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1),
    "should be one of"
  )
})

test_that("bootindices", {
  # Unclustered resampling: one set of row indices per group per replicate,
  # each as long as the group itself
  set.seed(1)
  flat <- bootindices(rep(c("a", "b"), c(5, 3)), NULL, boot.n = 10)
  expect_named(flat, c("a", "b"))
  expect_length(flat[["a"]], 10)
  expect_true(all(lengths(flat[["a"]]) == 5L))
  expect_true(all(lengths(flat[["b"]]) == 3L))
  expect_true(all(unlist(flat[["a"]]) %in% seq_len(5)))

  # Crossed clusters: individuals span the three body regions, so every group
  # is built from the same draw of individuals, preserving their pairing
  by <- rep(c("B", "C", "T"), each = 7)
  ind <- rep(paste0("ind", 1:7), 3)

  set.seed(1)
  crossed <- bootindices(by, ind, boot.n = 20)
  expect_named(crossed, c("B", "C", "T"))
  expect_length(crossed[["B"]], 20)
  expect_true(all(lengths(crossed[["B"]]) == 7L))
  expect_identical(crossed[["B"]], crossed[["C"]])
  expect_identical(crossed[["C"]], crossed[["T"]])

  # ...which is exactly what asking for nested resampling switches off
  set.seed(1)
  forced <- bootindices(by, ind, boot.n = 20, nesting = "nested")
  expect_false(identical(forced[["B"]], forced[["C"]]))

  # Nested clusters: five individuals per group, four measurements each, so
  # replicates are always unions of whole four-row clusters
  bynest <- rep(c("pop1", "pop2"), each = 20)
  indnest <- paste0(bynest, "_ind", rep(rep(1:5, each = 4), 2))

  set.seed(1)
  nested <- bootindices(bynest, indnest, boot.n = 20)
  expect_true(all(lengths(nested[["pop1"]]) == 20L))
  expect_false(identical(nested[["pop1"]], nested[["pop2"]]))
  expect_true(all(vapply(
    nested[["pop1"]],
    function(i) all(table(ceiling(i / 4)) %% 4 == 0),
    logical(1)
  )))

  # Groups sharing only some of their clusters with the rest are never left
  # without any rows at all
  bypart <- rep(c("a", "b", "c"), c(3, 2, 3))
  indpart <- c("i1", "i2", "i3", "i1", "i2", "i1", "i2", "i3")

  set.seed(1)
  partial <- bootindices(bypart, indpart, boot.n = 200)
  expect_true(all(lengths(partial[["b"]]) > 0))
  expect_true(all(unlist(partial[["b"]]) %in% seq_len(2)))

  # Too few clusters to say anything useful
  expect_message(
    bootindices(rep(c("a", "b"), each = 3), rep(c("x", "y", "z"), 2), boot.n = 5),
    "[Ff]ewer than five clusters"
  )
})
