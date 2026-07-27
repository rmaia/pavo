#' Bootstrap colour distance confidence intervals
#'
#' Uses a bootstrap procedure to generate confidence intervals
#'  for the mean colour distance between two or more samples of colours
#'
#' @param vismodeldata (required) quantum catch colour data.
#'  Can be the result from [vismodel()], or [colspace()]. Data may also be
#'  independently calculated quantum catches, in the form of a data frame with
#'  columns representing photoreceptors.
#' @param by (required) a numeric or character vector indicating the group to which each row from
#'  the object belongs to.
#' @param boot.n number of bootstrap replicates (defaults to 1000)
#' @param alpha the confidence level for the confidence intervals (defaults to 0.95)
#' @param raw should the full set of bootstrapped distances (equal in length to boot.n)
#' be returned, instead of the summary distances and CI's? Defaults to FALSE.
#' Each row is one bootstrap replicate, so values sharing a row, whether for
#' different contrasts or for dS and dL, were calculated from the same resampled
#' data and can be compared with one another.
#' @param ... other arguments to be passed to [coldist()]. Must at minimum
#' include `n` and `weber`. See [coldist()] for details.
#' @param cluster an optional numeric or character vector, of the same length as
#'  `by`, identifying the higher-level unit (e.g. the individual, colony, or
#'  patch-bearing pattern) that each row belongs to. When supplied, resampling is
#'  done over whole clusters rather than over individual rows, which is
#'  appropriate whenever rows are not independent of one another. Defaults to
#'  NULL, in which case rows are resampled independently within each group, as in
#'  previous versions.
#' @param nesting the relationship between `cluster` and `by`, one of `"auto"`
#'  (the default), `"crossed"`, or `"nested"`. Under `"crossed"`, clusters span
#'  the levels of `by` (e.g. the same individual contributes a crown, throat and
#'  breast measurement) and a single draw of clusters is shared across groups,
#'  which preserves the pairing between them. Under `"nested"`, each cluster
#'  belongs to exactly one group (e.g. repeated measurements of an individual
#'  within a population) and clusters are drawn independently within each group.
#'  `"auto"` chooses between the two by checking whether any cluster appears
#'  under more than one level of `by`. Ignored when `cluster` is NULL.
#'
#' @param ci.type the type of confidence interval, either `"perc"` (the default)
#'  for percentiles of the bootstrap distribution, or `"bca"` for bias-corrected
#'  and accelerated limits. Colour distances are bounded below by zero and are
#'  usually right-skewed, which is the situation in which percentile limits sit
#'  off-centre; BCa shifts them to account for both that skew and for where the
#'  empirical distance falls within the bootstrap distribution. It costs one
#'  additional jackknife pass, leaving out a single row at a time, or a whole
#'  cluster at a time when `cluster` is given.
#'
#'  Note that `cluster`, `nesting` and `ci.type` follow `...`, and so must all be
#'  named in full when used.
#'
#' @inherit getspec details
#'
#' @return a matrix including the empirical mean and bootstrapped
#'  confidence limits for dS (and dL if `achromatic = TRUE`), or a data.frame
#'  of raw bootstraped dS (and dL if `achromatic = TRUE`) values equal in length to boot.n.
#'
#' @examples
#' \donttest{
#' # Run the receptor-noise limited model, using the visual phenotype
#' # of the blue tit
#' data(sicalis)
#' vm <- vismodel(sicalis, achromatic = "bt.dc", relative = FALSE)
#' gr <- gsub("ind..", "", rownames(vm))
#' bootcoldist(vm, by = gr, n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1)
#'
#' # These data are hierarchically structured, since each of the seven individuals
#' # contributes one crown, throat, and breast measurement. Rows sharing an
#' # individual are therefore not independent, and we can resample whole
#' # individuals rather than individual rows to account for it.
#' ind <- substr(rownames(vm), 1, 4)
#' bootcoldist(vm,
#'   by = gr, cluster = ind,
#'   n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1
#' )
#'
#' # Run the same again, though as a simple colourspace model
#' data(sicalis)
#' vm <- vismodel(sicalis, achromatic = "bt.dc")
#' space <- colspace(vm)
#' gr <- gsub("ind..", "", rownames(space))
#' bootcoldist(space, by = gr)
#'
#' # Estimate bootstrapped colour-distances for a more 'specialised' model,
#' # like the colour hexagon
#' data(flowers)
#' vis.flowers <- vismodel(flowers,
#'   visual = "apis", qcatch = "Ei", relative = FALSE,
#'   vonkries = TRUE, achromatic = "l", bkg = "green"
#' )
#' flowers.hex <- colspace(vis.flowers, space = "hexagon")
#' pop_group <- c(rep("pop_1", nrow(flowers.hex) / 2), rep("pop_2", nrow(flowers.hex) / 2))
#' bootcoldist(flowers.hex, by = pop_group)
#' }
#'
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom stats setNames pnorm qnorm
#'
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#'  Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}

bootcoldist <- function(vismodeldata, by, boot.n = 1000, alpha = 0.95, raw = FALSE, ...,
                        cluster = NULL, nesting = c("auto", "crossed", "nested"),
                        ci.type = c("perc", "bca")) {
  # These arguments deliberately sit after the dots, so that they have to be
  # named in full. Were they to come before, R's partial matching would bind
  # arguments meant for coldist() to them instead: coldist()'s 'n' is a prefix of
  # 'nesting', so bootcoldist(vm, by = gr, n = c(1, 2, 2, 4)) would silently pass
  # the receptor densities as the nesting structure.
  nesting <- match.arg(nesting)
  ci.type <- match.arg(ci.type)

  # Define an inner function to calculate the geometric mean
  gmean <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
    # If any of the values are negative, return NaN
    if (any(x < 0, na.rm = TRUE)) {
      return(NaN)
    }
    # If zero.propagate is TRUE and there are zeros in x, return 0
    if (zero.propagate) {
      if (any(x == 0, na.rm = TRUE)) {
        return(0)
      }
      # Otherwise, return the geometric mean
      exp(mean(log(x), na.rm = na.rm))
    } else {
      # If zero.propagate is FALSE, calculate the geometric mean excluding zeros
      # TODO: double check if we should take length(x) or length(x[x > 0]) for the denominator
      exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
  }

  # Convert any non-numeric columns to (nonsense) numeric values
  # They're not used anyway, and subsetting etc. strips attributes,
  # so this is just a simple/lazy workaround
  num_cols <- vapply(vismodeldata, is.numeric, logical(1))
  vismodeldata[, !num_cols] <- 0

  # Decide how each column is averaged when a group is summarised.
  #
  # Receptor-noise distances are linear in the log of the quantum catches, so
  # the centroid of a group of catches is their geometric mean. Distances
  # between colspace objects are measured in their coordinates directly, so
  # there the centroid is the arithmetic mean of those coordinates. Luminance
  # channels stay geometric, since achromatic contrast is a ratio.
  #
  # Coordinates are listed per space, and have to match the columns coldist()
  # reads for that space.
  spacecoordinates <- list(
    dispace = "x",
    trispace = c("x", "y"),
    tcs = c("x", "y", "z"),
    hexagon = c("x", "y"),
    categorical = c("x", "y"),
    CIEXYZ = c("x", "y"),
    CIELAB = c("L", "a", "b"),
    CIELCh = c("L", "a", "b"),
    coc = c("x", "y"),
    segment = c("MS", "LM")
  )

  coordinates <- NULL
  if (inherits(vismodeldata, "colspace")) {
    coordinates <- spacecoordinates[[attr(vismodeldata, "clrsp")]]
  }
  arithmetic <- names(vismodeldata) %in% coordinates

  # Summarise a set of rows down to one value per column
  groupsummary <- function(x) {
    x <- as.matrix(x)
    out <- vapply(seq_len(ncol(x)), function(i) {
      if (arithmetic[i]) mean(x[, i], na.rm = TRUE) else gmean(x[, i])
    }, numeric(1))
    names(out) <- colnames(x)
    out
  }

  # Start preparing the arguments

  arg0 <- list(...)

  # Check for achromatic argument. 'achromatic' was previously called 'achro',
  # so this handles backward compatibility.
  # TODO: add a warning about this so users update their scripts??
  if (is.null(arg0$achromatic)) {
    arg0$achromatic <- arg0$achro
  }

  # Determine if RN model should be used.
  # For non-colspace objects, the RN model is required.
  useRNmodel <- !inherits(vismodeldata, "colspace")

  if (is.null(arg0$achromatic)) {
    if (is.null(attr(vismodeldata, "visualsystem.achromatic"))) {
      stop('argument "achromatic" to be passed to "coldist" is missing', call. = FALSE)
    }

    if (attr(vismodeldata, "visualsystem.achromatic") == "none") {
      arg0$achromatic <- FALSE
    } else {
      arg0$achromatic <- TRUE
    }
  }

  # Only require n & webers if using RN model
  if (useRNmodel) {
    # Receptor density
    if (is.null(arg0$n)) {
      stop('argument "n" to be passed to "coldist" is missing', call. = FALSE)
    }
    # Chromatic weber fraction
    if (is.null(arg0$weber)) {
      stop('argument "weber" to be passed to "coldist" is missing', call. = FALSE)
    }
    # Noise type
    if (is.null(arg0$noise)) {
      arg0$noise <- "neural"
    }
    # Weber cone ref
    if (is.null(arg0$weber.ref)) {
      arg0$weber.ref <- "longest"
    }
    if (arg0$achromatic && is.null(arg0$weber.achro)) {
      stop('argument "weber.achro" to be passed to "coldist" is missing', call. = FALSE)
    }
  } else {
    arg0$weber <- NULL
    arg0$n <- NULL
    arg0$noise <- NULL
    arg0$weber.ref <- NULL
    arg0$weber.achro <- NULL
  }

  # Check if qcatch attribute exists, if not then stop the function with an error
  if (is.null(arg0$qcatch)) {
    if (is.null(attr(vismodeldata, "qcatch"))) {
      stop('argument "qcatch" to be passed to "coldist" is missing', call. = FALSE)
    }
    arg0$qcatch <- attr(vismodeldata, "qcatch")
  }

  # Validate the clustering variable, if one was given
  if (!is.null(cluster)) {
    if (length(cluster) != nrow(vismodeldata)) {
      stop('argument "cluster" must have one entry per row of "vismodeldata"',
        call. = FALSE
      )
    }
    if (anyNA(cluster)) {
      stop('argument "cluster" cannot contain missing values', call. = FALSE)
    }
    cluster <- as.character(cluster)
  }

  # Reorder the visual model data by group
  sortinggroups <- order(by)
  vismodeldata <- vismodeldata[sortinggroups, ]
  by <- by[sortinggroups]

  # The clustering variable is row-wise, so it has to follow the same reordering
  if (!is.null(cluster)) {
    cluster <- cluster[sortinggroups]
  }

  # Group-wise mean deltaS for the empirical data
  empgroupmeans <- do.call(rbind, lapply(unique(by), function(g) {
    groupsummary(vismodeldata[by == g, , drop = FALSE])
  }))
  row.names(empgroupmeans) <- unique(by)
  empgroupmeans <- as.data.frame(empgroupmeans)

  # Set the attributes for the grouped means
  datattributes <- grep("names", names(attributes(vismodeldata)),
    invert = TRUE, value = TRUE, fixed = TRUE
  )

  # Prepare empirical argument list and calculate empirical color distances
  attributes(empgroupmeans)[datattributes] <- attributes(vismodeldata)[datattributes]

  # Begin bootstrapping procedure
  # This involves sampling with replacement from the original data (group-wise), calculating
  # group-wise means for each bootstrap sample, and then performing the color distance calculation for each.
  # After obtaining the bootstrap color distance calculations, calculate confidence intervals and
  # possibly return raw bootstrap results.
  emparg <- arg0
  emparg$modeldata <- empgroupmeans

  empcd <- do.call(coldist, emparg)

  empdS <- setNames(empcd$dS, paste(empcd$patch1, empcd$patch2, sep = "-"))

  # separate data by group
  bygroup <- lapply(unique(by), function(x) vismodeldata[by == x, ])

  # split(dat, by) also works but is about twice as slow
  names(bygroup) <- unique(by)

  # create the row indices to sample, for every group and every replicate.
  # When 'cluster' is NULL this resamples rows independently within each group,
  # otherwise it resamples whole clusters. See bootindices() below.
  its <- bootindices(by, cluster, boot.n, nesting)

  # use the indices to break each group's data into bootstrap replicates
  # returns a list with length = number of by
  # each entry is itself a list with length = number of replicates
  bootbygroup <- lapply(seq_along(bygroup), function(x) {
    lapply(its[[x]], function(z) bygroup[[x]][z, , drop = FALSE])
  })

  # now take the column means for all bootstrapped by
  # returns a list with length = number of by
  # each row in these = the mean of bootstrap replicates
  groupcolmeans <- lapply(bootbygroup, function(z) {
    do.call(rbind, lapply(z, groupsummary))
  })

  # now "split and merge"
  # creating a list with length = number of bootstrap replicates
  # and rows in each entry = mean per group in that replicate
  bootgrouped <- lapply(seq_len(boot.n), function(x) {
    do.call(rbind, lapply(groupcolmeans, "[", x, )) # nolint: false positive
  })

  # ...name the rows by group
  bootgrouped <- lapply(bootgrouped, function(x) {
    row.names(x) <- unique(by)
    x
  })

  # ...and give them the necessary attributes
  bootgrouped <- lapply(bootgrouped, as.data.frame)
  attribs <- attributes(vismodeldata)
  attribs <- attribs[grep("data|names", names(attribs), invert = TRUE)]

  for (i in seq_along(bootgrouped)) {
    attributes(bootgrouped[[i]])[names(attribs)] <- attribs
  }

  # Creates temporary bootstrap function, which is applied to each bootstrap sample
  tmpbootcdfoo <- function(x) {
    tmparg <- arg0
    tmparg$modeldata <- x
    do.call(coldist, tmparg)
  }

  # Handles progress bar for running bootstrap calculations
  with_progress({
    p <- progressor(along = bootgrouped)
    bootcd <- future_lapply(bootgrouped, function(z) {
      p()
      tryCatch(suppressMessages(tmpbootcdfoo(z)),
        error = function(e) e
      )
    }, future.seed = TRUE)
  })

  checkreplicates(bootcd, boot.n)

  # Extract deltaS values from bootcd and restructure in one dataframe
  bootdS <- do.call(
    rbind,
    lapply(bootcd, function(x) {
      setNames(x$dS, paste(x$patch1, x$patch2, sep = "-"))
    })
  )

  # Backstop for a replicate that returned a different set of contrasts instead
  # of failing outright, which would leave the distances the wrong shape
  if (nrow(bootdS) < boot.n) {
    stop(
      "Bootstrap replicates did not all return the same colour distances.",
      call. = FALSE
    )
  }

  # Order, find quantiles, and set up deltaS confidence intervals
  probs <- (1 + c(-alpha, alpha)) / 2
  quantileindices <- round(boot.n * probs)

  # Too few replicates for the requested alpha rounds the lower index down to
  # zero, which drops a row rather than selecting one, and the interval then
  # fails to assemble with an error that says nothing useful.
  if (any(quantileindices < 1)) {
    stop(
      "boot.n (", boot.n, ") is too small to estimate a ", alpha,
      " confidence interval, since its lower limit falls below the first ",
      "bootstrap replicate. Increase boot.n to at least ",
      ceiling(2 / (1 - alpha)), " for this value of alpha.",
      call. = FALSE
    )
  }

  # For BCa limits, each contrast needs its own pair of percentiles, which in
  # turn need leave-one-out distances. The jackknife is skipped entirely for
  # plain percentile limits, and falls back to them if it cannot be computed.
  jack <- NULL
  if (ci.type == "bca") {
    jack <- jackdist(vismodeldata, by, cluster, groupsummary, attribs, arg0)
  }

  # Only the copy used for the quantiles is sorted. Sorting bootdS itself would
  # order every contrast independently, which breaks the correspondence between
  # them, and rows of the raw output are meant to be whole replicates.
  sorteddS <- apply(bootdS, 2, sort)

  # Ensure names match with empirical values (even though they should match already)
  dsCI <- bootlimits(
    sorteddS[, names(empdS), drop = FALSE], empdS,
    if (is.null(jack)) NULL else jack$dS[, names(empdS), drop = FALSE],
    probs
  )
  rownames(dsCI) <- c("dS.lwr", "dS.upr")

  # Define empirical deltaS mean
  dS.mean <- empdS

  # Combine empirical and bootstrap deltaS statistics into a results dataframe
  res <- t(rbind(dS.mean, dsCI))

  # If raw = TRUE, create a new dataframe with raw bootstrapped deltaS distances
  if (raw) {
    rawres <- as.data.frame(bootdS)
    names(rawres) <- paste0(names(rawres), "_dS")
  }

  # If achromatic = TRUE, calculate empirical and bootstrap statistics for achromatic distances (deltaL)
  if (arg0$achromatic) {
    empdL <- setNames(empcd$dL, paste(empcd$patch1, empcd$patch2, sep = "-"))

    bootdL <- do.call(
      rbind,
      lapply(bootcd, function(x) {
        setNames(x$dL, paste(x$patch1, x$patch2, sep = "-"))
      })
    )

    sorteddL <- apply(bootdL, 2, sort)

    # Ensure names match with empirical values (even though they should match already)
    dlCI <- bootlimits(
      sorteddL[, names(empdL), drop = FALSE], empdL,
      if (is.null(jack)) NULL else jack$dL[, names(empdL), drop = FALSE],
      probs
    )
    rownames(dlCI) <- c("dL.lwr", "dL.upr")

    # Define empirical deltaL mean
    dL.mean <- empdL

    # Combine empirical and bootstrap deltaL statistics into a results dataframe
    res <- cbind(res, t(rbind(dL.mean, dlCI)))

    # If raw = TRUE, create a new dataframe with raw bootstrapped deltaL distances
    if (raw) {
      bootdL <- as.data.frame(bootdL)
      names(bootdL) <- paste0(names(bootdL), "_dL")
      rawres <- cbind(rawres, bootdL)
    }
  }

  if (raw) {
    res <- rawres
  }

  res
}

# Check the results of the bootstrap replicates, where a replicate that failed
# is the condition that stopped it rather than a set of distances.
#
# One failure is enough to stop the whole run. The resamples that fail are not a
# random subset of them, being the awkward ones by definition, so quietly
# carrying on with those that survived would bias the distribution.
checkreplicates <- function(bootcd, boot.n) {
  failed <- vapply(bootcd, inherits, logical(1), what = "error")

  if (any(failed)) {
    stop(
      sum(failed), " of ", boot.n, " bootstrap replicates failed, the first ",
      "of them reporting: ", conditionMessage(bootcd[[which(failed)[1]]]),
      call. = FALSE
    )
  }

  invisible(bootcd)
}

# Confidence limits from a matrix of bootstrap distances, sorted within each
# column, with one column per pairwise contrast.
#
# Without jackknife values these are plain percentiles of the bootstrap
# distribution. With them, the limits are bias-corrected and accelerated: the
# bias term shifts the interval according to where the empirical distance sits
# within the bootstrap distribution, and the acceleration term, taken from the
# skewness of the leave-one-out distances, allows for the standard error of a
# distance changing with the size of that distance.
bootlimits <- function(bootvals, empvals, jackvals, probs) {
  boot.n <- nrow(bootvals)
  indices <- matrix(round(boot.n * probs), nrow = 2, ncol = ncol(bootvals))

  if (!is.null(jackvals)) {
    # Median bias, as the proportion of replicates below the empirical value.
    # Held away from zero and one, since either sends the correction to infinity
    # and takes the whole interval with it.
    below <- colMeans(bootvals < rep(empvals, each = boot.n))
    below <- pmin(pmax(below, 1 / (2 * boot.n)), 1 - 1 / (2 * boot.n))
    z0 <- qnorm(below)

    # Acceleration, from the skewness of the leave-one-out distances. Identical
    # jackknife values leave it undefined, in which case there is no skew to
    # correct for anyway
    deviations <- -sweep(jackvals, 2, colMeans(jackvals))
    accel <- colSums(deviations^3) / (6 * colSums(deviations^2)^1.5)
    accel[!is.finite(accel)] <- 0

    z <- qnorm(probs)
    adjusted <- vapply(seq_along(z0), function(i) {
      pnorm(z0[i] + (z0[i] + z) / (1 - accel[i] * (z0[i] + z)))
    }, numeric(2))

    # A contrast whose correction blows up keeps its uncorrected percentiles
    usable <- apply(is.finite(adjusted), 2, all)
    indices[, usable] <- round(boot.n * adjusted[, usable, drop = FALSE])
  }

  indices <- pmin(pmax(indices, 1L), boot.n)

  limits <- vapply(
    seq_len(ncol(bootvals)),
    function(i) bootvals[indices[, i], i],
    numeric(2)
  )
  colnames(limits) <- colnames(bootvals)
  limits
}

# Leave-one-out colour distances, for the acceleration term of a BCa interval.
# The unit left out is a single row, or a whole cluster where one is given, to
# match whatever the bootstrap itself is resampling.
#
# Returns NULL, with a warning, if any unit cannot be left out without emptying
# one of the groups, since there is then no jackknife estimate to be had.
jackdist <- function(vismodeldata, by, cluster, groupsummary, attribs, arg0) {
  groups <- unique(by)
  # Without a clustering variable, every row is its own unit. Written out rather
  # than as x %||% y, which needs R 4.4 and so is off limits here
  unitof <- cluster
  if (is.null(unitof)) {
    unitof <- as.character(seq_along(by))
  }
  units <- unique(unitof)

  groupmean <- function(rows) groupsummary(vismodeldata[rows, , drop = FALSE])

  # Leaving out a unit only changes the groups it contributed to, so the rest of
  # the group means carry over untouched
  empirical <- do.call(rbind, lapply(groups, function(g) groupmean(by == g)))
  rownames(empirical) <- groups

  jackmeans <- lapply(units, function(u) {
    left.out <- unitof == u
    means <- empirical
    for (g in unique(by[left.out])) {
      keep <- by == g & !left.out
      if (!any(keep)) {
        return(NULL)
      }
      means[match(g, groups), ] <- groupmean(keep)
    }
    means
  })

  if (any(vapply(jackmeans, is.null, logical(1)))) {
    warning(
      "Accelerated limits need every group to survive leaving out one ",
      "observation, and at least one group here does not. Falling back to ",
      "percentile limits.",
      call. = FALSE
    )
    return(NULL)
  }

  jackcd <- lapply(jackmeans, function(x) {
    x <- as.data.frame(x)
    attributes(x)[names(attribs)] <- attribs

    tmparg <- arg0
    tmparg$modeldata <- x
    tryCatch(suppressMessages(do.call(coldist, tmparg)), error = function(e) e)
  })

  failed <- vapply(jackcd, inherits, logical(1), what = "error")

  if (any(failed)) {
    warning(
      "Jackknife colour distances could not be calculated, so percentile ",
      "limits were used in place of accelerated ones. The first failure ",
      "reported: ", conditionMessage(jackcd[[which(failed)[1]]]),
      call. = FALSE
    )
    return(NULL)
  }

  bind <- function(what) {
    do.call(rbind, lapply(jackcd, function(x) {
      setNames(x[[what]], paste(x$patch1, x$patch2, sep = "-"))
    }))
  }

  list(
    dS = bind("dS"),
    dL = if (isTRUE(arg0$achromatic)) bind("dL") else NULL
  )
}

# Generate the row indices used by each bootstrap replicate.
#
# Returns a list with length = number of groups (in the order given by
# unique(by)), each entry itself a list with length = boot.n, containing the
# positions of the sampled rows *within that group's block of data*.
#
# `by` and `cluster` are assumed to have already been reordered by order(by).
bootindices <- function(by, cluster, boot.n, nesting = "auto") {
  groups <- unique(by)
  samplesizes <- table(by)

  # No clustering variable, so rows are resampled independently within each
  # group. The sampling call is deliberately left as it was in earlier versions
  # of pavo, so that results generated under a given seed are unchanged.
  if (is.null(cluster)) {
    its <- lapply(samplesizes, function(x) sample.int(x, x * boot.n, replace = TRUE))
    its <- lapply(names(samplesizes), function(g) {
      split(its[[g]], rep(seq_len(boot.n), each = samplesizes[[g]]))
    })
    names(its) <- names(samplesizes)
    return(its[as.character(groups)])
  }

  # Row positions within each group's block, keyed by cluster. Clusters that
  # contribute nothing to a group are simply absent from that group's entry.
  # This list is kept in the order of unique(by), and is indexed by position
  # rather than by name, since 'by' itself may be numeric.
  rowsbycluster <- lapply(groups, function(g) {
    split(seq_len(sum(by == g)), cluster[by == g])
  })

  # Do clusters span the levels of 'by' (crossed), or does each belong to a
  # single level (nested)?
  if (nesting == "auto") {
    spread <- vapply(
      split(as.character(by), cluster),
      function(x) length(unique(x)) > 1L,
      logical(1)
    )
    nesting <- if (any(spread)) "crossed" else "nested"
  }

  ids <- unique(cluster)

  if (nesting == "crossed") {
    # A single draw of clusters per replicate, shared across all groups, which
    # preserves the pairing of observations within a cluster.
    #
    # A draw is rejected if it leaves any group with no rows at all, which can
    # happen when a group is represented by only a handful of clusters.
    present <- lapply(rowsbycluster, names)
    complete <- function(x) all(vapply(present, function(p) any(x %in% p), logical(1)))

    draws <- vector("list", boot.n)
    attempts <- 0L
    maxattempts <- 10L * boot.n

    for (i in seq_len(boot.n)) {
      repeat {
        drawn <- sample(ids, length(ids), replace = TRUE)
        if (complete(drawn)) break
        attempts <- attempts + 1L
        if (attempts > maxattempts) {
          stop(
            "Cluster resampling repeatedly left one or more groups empty. ",
            "Some group(s) are represented by too few clusters to bootstrap.",
            call. = FALSE
          )
        }
      }
      draws[[i]] <- drawn
    }

    its <- lapply(rowsbycluster, function(have) {
      lapply(draws, function(d) unlist(have[d[d %in% names(have)]], use.names = FALSE))
    })
  } else {
    # Clusters sit inside a single group, so each group's clusters are drawn
    # independently of the others.
    its <- lapply(rowsbycluster, function(have) {
      replicate(boot.n,
        unlist(have[sample(names(have), length(have), replace = TRUE)], use.names = FALSE),
        simplify = FALSE
      )
    })
  }

  names(its) <- groups

  # The effective sample size is now the number of clusters, not the number of
  # rows, so flag the cases where there are too few of them to say much.
  nclusters <- if (nesting == "crossed") {
    length(ids)
  } else {
    min(lengths(rowsbycluster))
  }
  if (nclusters < 5) {
    message(
      "Fewer than five clusters are available for resampling, so the ",
      "resulting confidence intervals should be interpreted with caution."
    )
  }

  its
}
