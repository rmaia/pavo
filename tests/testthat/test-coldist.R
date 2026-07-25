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

  # ...as does round(20 * 0.025), since R rounds halves to even
  expect_error(
    suppressWarnings(bootcoldist(
      vm,
      by = gr, n = c(1, 2, 3), weber = 0.1, weber.achro = 0.1, boot.n = 20
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
  expect_equal(clustered[, "dS.mean"], flat[, "dS.mean"])
  expect_equal(clustered[, "dL.mean"], flat[, "dL.mean"])

  # Confidence limits are still bracketing, and the resampling is reproducible
  expect_true(all(clustered[, "dS.lwr"] <= clustered[, "dS.upr"]))
  set.seed(1)
  again <- suppressWarnings(bootcoldist(
    vm,
    by = gr, cluster = ind, n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))
  expect_equal(again, clustered)

  # Clusters can also be supplied as a factor or as numbers
  set.seed(1)
  asfactor <- suppressWarnings(bootcoldist(
    vm,
    by = gr, cluster = factor(ind), n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  ))
  expect_equal(asfactor, clustered)

  # Nested designs, where each individual belongs to a single group
  grp <- ifelse(ind %in% paste0("ind", 1:3), "g1", "g2")
  nested <- suppressMessages(suppressWarnings(bootcoldist(
    vm,
    by = grp, cluster = ind, n = c(1, 2, 3),
    weber = 0.1, weber.achro = 0.1, boot.n = 50
  )))
  expect_identical(dim(nested), c(1L, 6L))
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
