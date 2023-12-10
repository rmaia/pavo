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
#' @param ... other arguments to be passed to [coldist()]. Must at minimum
#' include `n` and `weber`. See [coldist()] for details.
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
#' @importFrom stats aggregate setNames
#'
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#'  Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}

bootcoldist <- function(vismodeldata, by, boot.n = 1000, alpha = 0.95, raw = FALSE, ...) {
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
      exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
  }

  # Convert any non-numeric columns to (nonsense) numeric values
  # They're not used anyway, and subsetting etc. strips attributes,
  # so this is just a simple/lazy workaround
  num_cols <- unlist(lapply(vismodeldata, is.numeric))
  vismodeldata[, !num_cols] <- 0

  # Rescale any x-y-z positional data by adding a constant
  # Only applies to colspace objects, since (unlike for RN data) colour distances
  # are calculated based on coordinates, rather than qcatches. But this
  # caused problems when calculating geometric means, because coordinate
  # data often contain negative values. So this re-scales all coordinate
  # systems so they can never be negative, without affecting
  # the distances between points.
  vismodeldata[intersect(names(vismodeldata), c("x", "y", "z"))] <-
    vismodeldata[intersect(names(vismodeldata), c("x", "y", "z"))] + 100

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

  # Reorder the visual model data by group
  sortinggroups <- order(by)
  vismodeldata <- vismodeldata[sortinggroups, ]
  by <- by[sortinggroups]

  # Sample sizes for each group
  samplesizes <- table(by)

  # Group-wise geometric mean deltaS for the empirical data
  empgroupmeans <- aggregate(vismodeldata, list(by), gmean, simplify = TRUE)
  row.names(empgroupmeans) <- empgroupmeans[, 1]
  empgroupmeans <- empgroupmeans[, -1]

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

  # create vectors of indices to sample
  its <- lapply(samplesizes, function(x) sample.int(x, x * boot.n, replace = TRUE))

  # use the indices from its to sample from the data
  # returns a list with length = number of by
  # and rows = (sample size for that group) * (the number of bootstrap replicates) in each
  bootsamples <- lapply(seq_along(bygroup), function(x) bygroup[[x]][its[[x]], ])

  # next, split by bootstrap replicate
  # preserving the same sample size as that original group had
  #
  # list with length = number of by
  # and values = index for the bootstrap replicate that sample belongs to
  bootindex <- lapply(samplesizes, function(x) as.character(rep(seq_len(boot.n), each = x)))

  # use the index to break samples into bootstrap replicates
  # returns a list with length = number of by
  # each entry is itself a list with length = number of replicates
  bootbygroup <- lapply(seq_along(bygroup), function(x) {
    lapply(unique(bootindex[[x]]), function(z) bootsamples[[x]][bootindex[[x]] == z, ])
  })

  # now take the column means for all bootstrapped by
  # returns a list with length = number of by
  # each row in these = the (geometric) mean of bootstrap replicates
  groupcolmeans <- lapply(bootbygroup, function(z) {
    do.call(rbind, lapply(z, function(x) apply(x, 2, gmean))) # nolint
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
        error = function(e) NULL
      )
    }, future.seed = TRUE)
  })

  # Extract deltaS values from bootcd and restructure in one dataframe
  bootdS <- do.call(
    rbind,
    lapply(bootcd, function(x) {
      setNames(x$dS, paste(x$patch1, x$patch2, sep = "-"))
    })
  )

  # Error handling: if the dimension of bootdS is less than boot.n,
  # stop function and print error message
  if (dim(bootdS)[1] < boot.n) {
    stop("Bootstrap sampling encountered errors.", call. = FALSE)
  }

  # Order, find quantiles, and set up deltaS confidence intervals
  quantileindices <- round(boot.n * ((1 + c(-alpha, alpha)) / 2))
  bootdS <- apply(bootdS, 2, sort)
  dsCI <- bootdS[quantileindices, , drop = FALSE]
  rownames(dsCI) <- c("dS.lwr", "dS.upr")

  # Ensure names match with empirical values (even though they should match already)
  dsCI <- dsCI[, names(empdS), drop = FALSE]

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

    bootdL <- apply(bootdL, 2, sort)
    dlCI <- bootdL[quantileindices, , drop = FALSE]
    rownames(dlCI) <- c("dL.lwr", "dL.upr")

    # Ensure names match with empirical values (even though they should match already)
    dlCI <- dlCI[, names(empdL), drop = FALSE]

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
