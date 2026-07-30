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
#'  Crossed resampling assumes the crossing is complete, so that every group of a
#'  contrast holds every cluster. Where only some clusters are shared, a group
#'  holding a subset of them contributes a varying number of rows from one
#'  replicate to the next, and its interval will be wider than it should be. Give
#'  such designs one contrast at a time, or label clusters so that groups which do
#'  not genuinely share individuals do not appear to. Reusing the labels
#'  `ind1`, `ind2`, ... within each of several populations is the usual way this
#'  happens by accident.
#' @param ci.type the type of confidence interval, either `"perc"` (the default)
#'  for percentiles of the bootstrap distribution, or `"bca"` for bias-corrected
#'  and accelerated limits. Colour distances are bounded below by zero and are
#'  usually right-skewed, which is the situation in which percentile limits sit
#'  off-centre. `bca` shifts them to account for both that skew and for where the
#'  empirical distance falls within the bootstrap distribution.
#' @param correct logical. Should the distance be corrected for the sampling
#'  error in the group means? Defaults to `FALSE` for consistency with previous
#'  versions, but `TRUE` is recommended wherever it is available, since the
#'  uncorrected distance is biased upwards for any data at all. Both the estimate
#'  and its interval move downwards, so a contrast will less often have its lower
#'  limit above a given threshold. The interval is if anything slightly wider,
#'  since the correction is estimated rather than known and the bootstrap carries
#'  that uncertainty as well.
#'
#'  The distance between two group means is biased upwards, because each mean is
#'  estimated with error and distance is a convex function of that error. On the
#'  squared scale that displacement is the mean squared pairwise distance among a
#'  group's observations divided by twice their number, summed over the two
#'  groups, so it is largest when groups are small and internally variable and it
#'  does not vanish as the true separation goes to zero. Two samples drawn from a
#'  single population will therefore be separated by an apparently non-zero
#'  distance. Setting `correct = TRUE` subtracts the displacement from the
#'  empirical distance and from every bootstrap replicate, using in each case the
#'  observations that replicate drew, and returns the square root of what
#'  remains. A distance that would turn negative becomes zero, in the same way
#'  and for the same reason as a negative variance component.
#'
#'  The subtraction is exactly unbiased on the squared scale. Taking the square
#'  root of an unbiased estimate of a squared distance is not itself unbiased,
#'  and errs slightly low, so a corrected distance is a little conservative.
#'
#'  Where `cluster` is given and the design is crossed, so that the same
#'  individuals contribute to both groups of a contrast, the two group means are
#'  correlated and their covariance belongs in the displacement. The correction
#'  then works from the differences between each individual's own pair of
#'  measurements, which carries that covariance. Treating the groups as
#'  independent in this case would subtract far too much. Designs in which only
#'  some individuals are shared between two groups are refused, since neither
#'  estimator applies. Exact unbiasedness assumes clusters of roughly equal size,
#'  because the group mean is taken over rows while the displacement is estimated
#'  over clusters. Under marked imbalance the two no longer agree: the
#'  between-cluster part of the displacement is under-corrected and the
#'  within-cluster part over-corrected, so the net direction depends on which
#'  source of variation dominates and the departure is not necessarily small.
#'  Where cluster sizes are very uneven, treat the result as approximate.
#'
#'  The correction relies on the distance being one that arises from an inner
#'  product, and so is unavailable with `noise = "quantum"`, in the `CIELAB`,
#'  `CIELCh` and `coc` spaces, and for achromatic contrast in a colourspace
#'  model, where luminance contrast is a ratio rather than a distance. It cannot
#'  currently be combined with `ci.type = "bca"`.
#'
#'  A design in which clusters span the levels of `by` is crossed whatever
#'  `nesting` says, so `correct = TRUE` refuses `nesting = "nested"` in that
#'  case rather than treating correlated group means as independent.
#'
#'  Note that `cluster`, `nesting`, `ci.type` and `correct` follow `...`, and so
#'  must all be named in full when used.
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
#' # The distances themselves are still inflated, since each group mean is
#' # estimated from only seven birds and the distance between two noisy means
#' # exceeds the distance between the true ones. correct = TRUE removes that
#' # displacement, and every contrast shrinks.
#' bootcoldist(vm,
#'   by = gr, cluster = ind, correct = TRUE,
#'   n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1
#' )
#'
#' # These data are crossed, since every bird supplies all three patches, so the
#' # three group means are correlated with one another. Supplying cluster, as
#' # above, lets the correction work from each bird's own differences between
#' # patches, which carries that shared bird-level variation. Omitting it treats
#' # the groups as independent samples and removes more than it should, so
#' # compare the two and prefer the clustered one.
#' bootcoldist(vm,
#'   by = gr, correct = TRUE,
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
                        ci.type = c("perc", "bca"), correct = FALSE) {
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

  # Filled in once qcatch is known, below. groupsummary() is only called later.
  arithmetic <- NULL

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

  # Normalise once. Downstream this value is tested both as `arg0$achromatic` and
  # as `isTRUE(arg0$achromatic)`, and a truthy non-logical such as 1 would take
  # different branches in the two places, building no achromatic correction setup
  # and then asking for it. Coerce rather than use isTRUE(), which is FALSE for
  # 1 and would silently drop the achromatic channel instead of keeping it.
  achro <- as.logical(arg0$achromatic)
  if (length(achro) != 1L || is.na(achro)) {
    stop("`achromatic` must be a single TRUE or FALSE.", call. = FALSE)
  }
  arg0$achromatic <- achro

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

  # coldist() takes qcatch from the object's own attribute for vismodel and
  # colspace input, overriding whatever was passed. Resolve it the same way here,
  # so that the correction's view of how the catches were transformed cannot
  # disagree with what coldist() actually did. Reading only the argument would
  # let a user pass qcatch = "Qi" over an "fi" object and have the displacement
  # computed from the log of already-logged values.
  # Gate on class, not on the attribute. A plain data frame can carry a qcatch
  # attribute, since as.data.frame() drops the subclass but keeps everything else,
  # and coldist() honours the argument for those. Testing the attribute would
  # override the argument for such input and silently change the distances.
  fromobject <- if (inherits(vismodeldata, c("vismodel", "colspace"))) {
    attr(vismodeldata, "qcatch")
  } else {
    NULL
  }
  if (!is.null(fromobject)) {
    arg0$qcatch <- fromobject
  } else if (is.null(arg0$qcatch)) {
    stop('argument "qcatch" to be passed to "coldist" is missing', call. = FALSE)
  }

  # Which columns take an arithmetic mean rather than a geometric one. The test
  # is whether the column already lives in the space the distance is measured in,
  # because the centroid of a group is the ordinary mean there.
  #
  # Colourspace coordinates do. So do log-transformed quantum catches. coldist()
  # logs "Qi" on the way in and leaves "fi" alone, so "fi" values are already on
  # the log scale and their centroid is their arithmetic mean. A geometric mean of
  # them is not the centroid of anything, and because it does not commute with
  # adding a constant it made the reported distance depend on the factor the
  # illuminant happened to be scaled by. Scaling adds a constant to every log
  # catch, which is a pure intensity shift and lies in the null space of the
  # chromatic metric, so chromatic distance must be invariant to it.
  arithmetic <- names(vismodeldata) %in% coordinates |
    identical(arg0$qcatch, "fi")


  # The sampling-error correction is only defined where the distance is a norm
  # arising from an inner product, since it rests on the identity
  #   sum_{i<j} ||u_i - u_j||^2 = m * sum_i ||u_i - mean||^2
  # which fails otherwise. That rules out quantum noise, whose per-receptor term
  # depends on the pair being compared; CIE2000, which is not a metric of that
  # kind; and the Manhattan distances of the colour-opponent space.
  if (correct) {
    clrsp <- attr(vismodeldata, "clrsp")

    # coldist() runs match.arg() on 'noise', so an abbreviation such as "q"
    # reaches the quantum model. Resolve it the same way before testing, or the
    # guard sits out and a neural-noise displacement is subtracted from a
    # quantum-noise distance.
    noisetype <- if (is.null(arg0$noise)) {
      NA_character_
    } else {
      match.arg(arg0$noise, c("neural", "quantum"))
    }

    if (identical(noisetype, "quantum")) {
      stop(
        'correct = TRUE is not available with noise = "quantum", because the ',
        "receptor noise then depends on the pair of colours being compared and ",
        "the distance is no longer a fixed quadratic form.",
        call. = FALSE
      )
    }
    # The correction assumes the group centroid is the arithmetic mean in the
    # space the metric acts on. With qcatch = "Qi" the geometric mean of catches
    # is exactly that, being the arithmetic mean of their logs. With "fi" the
    # values are already logged, so the geometric mean groupsummary() takes is
    # not the centroid the displacement belongs to.
    if (!is.null(clrsp) && clrsp %in% c("CIELAB", "CIELCh", "coc")) {
      stop(
        "correct = TRUE is not available in the ", clrsp, " space, whose ",
        "distances do not arise from an inner product.",
        call. = FALSE
      )
    }
    if (!is.null(clrsp) && isTRUE(arg0$achromatic)) {
      stop(
        "correct = TRUE cannot be combined with achromatic = TRUE for a ",
        "colspace object, because luminance contrast is then a ratio rather ",
        "than a distance. Either set achromatic = FALSE, or pass the ",
        "vismodel object rather than the colspace one.",
        call. = FALSE
      )
    }
    if (identical(ci.type, "bca")) {
      stop(
        'correct = TRUE cannot yet be combined with ci.type = "bca". The ',
        "corrected distance is bounded below by zero and piles up there when ",
        "groups are close, which leaves the bias term of an accelerated ",
        'interval undefined. Use ci.type = "perc".',
        call. = FALSE
      )
    }
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

  # Attributes carried onto every set of group means handed back to coldist().
  # Defined here rather than further down because the correction needs them too.
  attribs <- attributes(vismodeldata)
  attribs <- attribs[grep("data|names", names(attribs), invert = TRUE)]

  # separate data by group
  bygroup <- lapply(unique(by), function(x) vismodeldata[by == x, ])

  # split(dat, by) also works but is about twice as slow
  names(bygroup) <- unique(by)

  # create the row indices to sample, for every group and every replicate.
  # When 'cluster' is NULL this resamples rows independently within each group,
  # otherwise it resamples whole clusters. See bootindices() below.
  its <- bootindices(by, cluster, boot.n, nesting)

  # Machinery for the sampling-error correction. The displacement of the squared
  # centroid distance is the sum over groups of the mean squared pairwise
  # distance among that group's resampling units, divided by twice their number.
  # Every replicate draws from the same pool of units, so the pairwise distances
  # are computed once per group here and each replicate then only needs a
  # quadratic form in how many times it drew each unit. See correctionsetup() below.
  if (correct) {
    crossed <- identical(resolvenesting(by, cluster, nesting), "crossed")

    # A design declared nested that is not nested is the one way to get a
    # silently wrong answer here. Treating correlated group means as independent
    # subtracts both groups' scatter instead of the scatter of their differences,
    # which removes roughly twice what it should and can floor a real separation
    # to zero without any warning. The declaration is honoured for resampling,
    # where it is merely a choice, but not for the correction, where it is a
    # statement about the data that can be checked.
    if (!crossed &&
        identical(resolvenesting(by, cluster, "auto"), "crossed")) {
      stop(
        'nesting = "nested" was given, but at least one cluster appears under ',
        "more than one level of `by`, so the group means being compared are not ",
        "independent. The correction would remove far more than it should. Use ",
        'nesting = "crossed", or drop `correct = TRUE`.',
        call. = FALSE
      )
    }

    channels <- if (isTRUE(arg0$achromatic)) c("dS", "dL") else "dS"

    # The metric each channel is measured in. Chromatic distance is the fixed
    # quadratic form of the receptor-noise model, or plain Euclidean distance in
    # the colourspace coordinates. Achromatic contrast is an absolute difference
    # in log luminance over a Weber fraction, so a one-dimensional metric.
    metrics <- list(dS = if (is.null(coordinates)) {
      rnlmatrix(arg0$n, arg0$weber, arg0$weber.ref)
    } else {
      diag(1, length(coordinates))
    })
    if ("dL" %in% channels) metrics$dL <- matrix(1 / arg0$weber.achro^2, 1, 1)

    # coldist() logs quantum catches and reports how many cones it used; the
    # luminance channel, when there is one, is the last column.
    tolog <- is.null(coordinates) && identical(arg0$qcatch, "Qi")
    dscols <- if (is.null(coordinates)) seq_len(attr(empcd, "ncone")) else coordinates
    contrasts <- cbind(empcd$patch1, empcd$patch2)

    setup <- correctionsetup(
      bygroup, by, cluster, groupsummary, crossed, contrasts,
      metrics = metrics["dS"], cols = dscols, tolog = tolog
    )
    if ("dL" %in% channels) {
      lumsetup <- correctionsetup(
        bygroup, by, cluster, groupsummary, crossed, contrasts,
        metrics = metrics["dL"], cols = ncol(vismodeldata), tolog = tolog
      )
    }

    unitcounts <- lapply(seq_along(bygroup), function(g) {
      rep(1, nrow(resamplingunits(
        bygroup[[g]], if (is.null(cluster)) NULL else cluster[by == unique(by)[g]],
        groupsummary
      )))
    })
    empdisp <- displacement(setup, unitcounts, "dS")
    empdS <- sqrt(pmax(empdS^2 - drop(empdisp), 0))
  }

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

  # ...and give them the necessary attributes (see 'attribs' above)
  bootgrouped <- lapply(bootgrouped, as.data.frame)

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

  # Each replicate is corrected by the displacement of its own resample, not by
  # a single figure taken from the observed data. Subtracting a constant would
  # shift the interval without changing its shape, and the shape is part of what
  # is wrong: the correction is itself estimated, and the bootstrap has to see
  # that variability to reproduce the sampling distribution of the corrected
  # statistic.
  if (correct) {
    replicatecounts <- function(b) {
      lapply(seq_along(bygroup), function(g) {
        drawn <- attr(its[[g]][[b]], "unitdraw")
        if (is.null(drawn)) drawn <- its[[g]][[b]]
        tabulate(drawn, nbins = length(unitcounts[[g]]))
      })
    }

    # One displacement per contrast per replicate, matching the shape of bootdS.
    # vapply returns a bare vector when there is a single contrast, so the
    # result is reshaped explicitly rather than transposed.
    perreplicate <- function(setup, channel, ncontrast) {
      d <- vapply(seq_len(boot.n), function(b) {
        drop(displacement(setup, replicatecounts(b), channel))
      }, numeric(ncontrast))
      matrix(d, nrow = boot.n, ncol = ncontrast, byrow = TRUE)
    }

    bootdisp <- perreplicate(setup, "dS", ncol(bootdS))
    bootdS <- sqrt(pmax(bootdS^2 - bootdisp, 0))
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

  # NA would be silently dropped by the sort below. If every contrast lost the
  # same number, apply() would return a shorter matrix and the limits would be
  # read from the wrong order statistics without any complaint, so this has to be
  # caught rather than left to surface as a dimension error further on.
  # sort() drops NA silently. If every column loses the same count, apply() returns
  # a shorter matrix and the limits come from the wrong order statistics with no
  # complaint; if the loss is ragged it returns a list and fails obscurely later.
  checkfinite <- function(x, channel) {
    if (anyNA(x)) {
      stop(
        sum(is.na(x)), " of ", length(x), " bootstrapped ", channel,
        " values are NA, so confidence limits cannot be taken from them.",
        if (identical(channel, "dL")) {
          " This usually means achromatic = TRUE was passed for a model carrying no achromatic channel."
        } else {
          " This usually means some resampled group had a non-positive quantum catch."
        },
        call. = FALSE
      )
    }
    invisible(x)
  }
  checkfinite(bootdS, "dS")

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

    # Achromatic contrast under the receptor-noise model is the absolute
    # difference in log luminance over a Weber fraction, which is a distance in
    # one dimension and so takes the same correction. The ratio-based luminance
    # contrasts of the colourspace models do not, and are refused above.
    if (correct) {
      empdispL <- displacement(lumsetup, unitcounts, "dL")
      empdL <- sqrt(pmax(empdL^2 - drop(empdispL), 0))

      bootdispL <- perreplicate(lumsetup, "dL", ncol(bootdL))
      bootdL <- sqrt(pmax(bootdL^2 - bootdispL, 0))
    }

    checkfinite(bootdL, "dL")
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

# Is each cluster confined to one level of 'by' (nested), or does it span them
# (crossed)? Both the resampling and the sampling-error correction need to know,
# so the decision is made once and shared rather than taken twice.
resolvenesting <- function(by, cluster, nesting = "auto") {
  if (is.null(cluster)) {
    return("nested")
  }
  if (!identical(nesting, "auto")) {
    return(nesting)
  }
  spread <- vapply(
    split(as.character(by), cluster),
    function(x) length(unique(x)) > 1L,
    logical(1)
  )
  if (any(spread)) "crossed" else "nested"
}

# The matrix behind the receptor-noise distance.
#
# Under neural noise the per-receptor noise term does not depend on the pair of
# colours being compared, so dS^2 is a fixed quadratic form in the difference of
# log quantum catches. Writing w_i for the reciprocal squared noise of receptor
# i, that form is diag(w) - w w' / sum(w), which is to say dS^2 is a weighted
# variance of the log-catch differences. Its rank is one less than the number of
# receptors, the null direction being a common scaling of all catches.
rnlmatrix <- function(n, weber, weber.ref = "longest") {
  reln <- n / sum(n)
  ref <- if (identical(weber.ref, "longest")) length(n) else weber.ref
  v <- if (length(weber) == length(n)) weber * sqrt(reln) else weber * sqrt(reln[ref])
  e <- v / sqrt(reln)
  w <- 1 / e^2
  diag(w, length(w)) - tcrossprod(w) / sum(w)
}

# The units that resampling draws from: single rows when there is no clustering
# variable, and per-cluster means when there is one.
resamplingunits <- function(rows, cluster, groupsummary) {
  if (is.null(cluster)) {
    return(rows)
  }
  do.call(rbind, lapply(
    split(seq_len(nrow(rows)), cluster),
    function(i) groupsummary(rows[i, , drop = FALSE])
  ))
}

# Squared distances between every pair of rows of X under the metric A.
sqdistmatrix <- function(X, A) {
  q <- rowSums((X %*% A) * X)
  out <- outer(q, q, "+") - 2 * X %*% A %*% t(X)
  out[out < 0] <- 0
  out
}

# Set up the sampling-error correction, one entry per contrast.
#
# The displacement of a squared centroid distance is tr(A Var(xbarA - xbarB)).
# Where the two groups are made up of different individuals that separates into
# a term per group. Where the same individuals contribute to both, as when every
# animal supplies a measurement of each patch being compared, the two centroids
# are correlated and the covariance between them has to come off as well. Adding
# the two group terms and stopping there subtracts far too much, by a factor
# that grows with the ratio of between- to within-individual variation.
#
# So a crossed contrast is handled by differencing. Take each individual's pair
# of measurements, difference them, and the displacement is the scatter of those
# differences over their number, which is the paired quantity and carries the
# covariance automatically.
#
# Each entry holds squared distances among whatever units that contrast is built
# from, so that a replicate's displacement is a quadratic form in how many times
# it drew each unit.
correctionsetup <- function(bygroup, by, cluster, groupsummary, crossed,
                            contrasts, metrics, cols, tolog) {
  groups <- unique(by)

  # `cols` selects the columns the metric acts on: the cone catches for
  # chromatic distance, the luminance channel for achromatic, or the named
  # coordinates of a colourspace. `tolog` matches coldist(), which logs quantum
  # catches but leaves colourspace coordinates alone.
  asmetric <- function(x) {
    x <- as.matrix(x[, cols, drop = FALSE])
    if (tolog) log(x) else x
  }

  unitsof <- function(g) {
    rows <- bygroup[[g]]
    cl <- if (is.null(cluster)) NULL else cluster[by == groups[g]]
    u <- resamplingunits(rows, cl, groupsummary)
    list(x = asmetric(u), ids = if (is.null(cl)) NULL else sort(unique(cl)))
  }

  units <- lapply(seq_along(groups), unitsof)

  lapply(seq_len(nrow(contrasts)), function(k) {
    g1 <- match(contrasts[k, 1], groups)
    g2 <- match(contrasts[k, 2], groups)

    if (crossed) {
      shared <- intersect(units[[g1]]$ids, units[[g2]]$ids)
      if (!setequal(units[[g1]]$ids, shared) || !setequal(units[[g2]]$ids, shared)) {
        stop(
          "correct = TRUE needs every cluster to appear in both groups of a ",
          "contrast when the design is crossed, and contrast ",
          paste(contrasts[k, ], collapse = "-"), " is only partly crossed. ",
          "Either complete the design, or analyse the contrast on its own.",
          call. = FALSE
        )
      }
      d <- units[[g1]]$x[match(shared, units[[g1]]$ids), , drop = FALSE] -
        units[[g2]]$x[match(shared, units[[g2]]$ids), , drop = FALSE]
      parts <- list(list(D = lapply(metrics, function(A) sqdistmatrix(d, A)), group = g1))
    } else {
      parts <- lapply(c(g1, g2), function(g) {
        list(D = lapply(metrics, function(A) sqdistmatrix(units[[g]]$x, A)), group = g)
      })
    }

    for (p in parts) {
      if (nrow(p$D[[1]]) < 2L) {
        stop(
          "The sampling-error correction needs at least two resampling units ",
          "per group, and one group has ", nrow(p$D[[1]]), ".",
          call. = FALSE
        )
      }
    }
    parts
  })
}

# Displacement of each contrast's squared distance, for one resample.
#
# `counts[[g]]` says how many times each of group g's units was drawn. For a
# multiset of m units the mean squared pairwise distance is c'Dc / (m(m - 1)),
# since c'Dc counts every ordered pair, and the displacement is that mean over
# 2m. The identity behind it is
#   sum_{i<j} ||u_i - u_j||^2 = m * sum_i ||u_i - mean||^2
# so no separate estimate of the noise covariance is needed. Under a crossed
# design there is a single part, built from paired differences, and the same
# arithmetic applies to it.
displacement <- function(setup, counts, channels) {
  vapply(channels, function(channel) {
    vapply(setup, function(parts) {
      sum(vapply(parts, function(p) {
        cg <- counts[[p$group]]
        m <- sum(cg)
        if (m < 2L) {
          stop(
            "A bootstrap replicate drew fewer than two resampling units for ",
            "one group, so the sampling-error correction is undefined for it.",
            call. = FALSE
          )
        }
        drop(cg %*% p$D[[channel]] %*% cg) / (2 * m^2 * (m - 1))
      }, numeric(1)))
    }, numeric(1))
  }, numeric(length(setup)))
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

# Row positions for a draw of whole clusters, carrying a record of which
# clusters were drawn.
#
# The rows are all that the resampling itself needs. The 'unitdraw' attribute is
# for the sampling-error correction, which works at the level of the units being
# resampled rather than the rows: it holds the position, among the group's
# clusters, of each cluster in the draw, so a cluster drawn twice appears twice.
withunits <- function(have, drawn) {
  idx <- unlist(have[drawn], use.names = FALSE)
  attr(idx, "unitdraw") <- match(drawn, names(have))
  idx
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
      lapply(draws, function(d) withunits(have, d[d %in% names(have)]))
    })
  } else {
    # Clusters sit inside a single group, so each group's clusters are drawn
    # independently of the others.
    its <- lapply(rowsbycluster, function(have) {
      replicate(boot.n,
        withunits(have, sample(names(have), length(have), replace = TRUE)),
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
