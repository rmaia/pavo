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
#' @param ... other arguments to be passed to [coldist()]. Must at minimum
#' include `n` and `weber`. See [coldist()] for details.
#' @inheritParams getspec
#' 
#' @inherit getspec details
#'
#' @return a matrix including the empirical mean and bootstrapped
#'  confidence limits for dS (and dL if `achromatic = TRUE`).
#'
#' @examples
#' \dontrun{
#' data(sicalis)
#' vm <- vismodel(sicalis, achromatic = "bt.dc")
#' gr <- gsub("ind..", "", rownames(vm))
#' bootcoldist(vm, by = gr, n = c(1, 2, 2, 4), weber = 0.1, weber.achro = 0.1)
#' }
#'
#' @export
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
#' @importFrom stats aggregate setNames
#'
#' @references Maia, R., White, T. E., (2018) Comparing colors using visual models.
#'  Behavioral Ecology, ary017 \doi{10.1093/beheco/ary017}


bootcoldist <- function(vismodeldata, by, boot.n = 1000, alpha = 0.95,
                        cores = NULL, ...) {
  if (!is.vismodel(vismodeldata) && !is.colspace(vismodeldata)) {
    stop('object must be a "vismodel" or "colspace" result', call. = FALSE)
  }

  if (!missing(cores)) {
    warning("'cores' argument is deprecated. See ?future::plan for more info ",
            "about how you can choose your parallelisation strategy.", 
            call. = FALSE)
  }

  # geometric mean
  gmean <- function(x, na.rm = TRUE, zero.propagate = FALSE) {
    if (any(x < 0, na.rm = TRUE)) {
      return(NaN)
    }
    if (zero.propagate) {
      if (any(x == 0, na.rm = TRUE)) {
        return(0)
      }
      exp(mean(log(x), na.rm = na.rm))
    } else {
      exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
    }
  }

  # start actual function

  arg0 <- list(...)

  # 'achromatic' used to be called just 'achro' so let's work around it.
  # TODO: add a warning about this so users update their scripts??
  if (is.null(arg0$achromatic)) {
    arg0$achromatic <- arg0$achro
  }

  if (is.null(arg0$n)) {
    stop('argument "n" to be passed to "coldist" is missing', call. = FALSE)
  }

  if (is.null(arg0$weber)) {
    stop('argument "weber" to be passed to "coldist" is missing', call. = FALSE)
  }

  if (is.null(arg0$qcatch)) {
    if (is.null(attr(vismodeldata, "qcatch"))) {
      stop('argument "qcatch" to be passed to "coldist" is missing', call. = FALSE)
    }

    arg0$qcatch <- attr(vismodeldata, "qcatch")
  }

  if (is.null(arg0$achromatic)) {
    if (is.null(attr(vismodeldata, "visualsystem.achromatic"))) {
      stop('argument "achromatic" to be passed to "coldist" is missing', call. = FALSE)
    }

    if (attr(vismodeldata, "visualsystem.achromatic") == "none") {
      arg0$achromatic <- FALSE
    }

    if (attr(vismodeldata, "visualsystem.achromatic") != "none") {
      arg0$achromatic <- TRUE
    }
  }

  if (arg0$achromatic) {
    if (is.null(arg0$weber.achro)) {
      stop('argument "weber.achro" to be passed to "coldist" is missing', call. = FALSE)
    }
  }

  if (is.null(arg0$noise)) {
    arg0$noise <- "neural"
  }

  if (is.null(arg0$weber.ref)) {
    arg0$weber.ref <- "longest"
  }

  sortinggroups <- order(by)
  vismodeldata <- vismodeldata[sortinggroups, ]
  by <- by[sortinggroups]

  samplesizes <- table(by)

  # calculate empirical deltaS
  empgroupmeans <- aggregate(vismodeldata, list(by), gmean, simplify = TRUE)
  row.names(empgroupmeans) <- empgroupmeans[, 1]
  empgroupmeans <- empgroupmeans[, -1]

  # empcd <- coldist(empgroupmeans, ...)
  # empcd <- do.call(coldist, list(modeldata=empgroupmeans, arg0))

  datattributes <- grep("names", names(attributes(vismodeldata)),
    invert = TRUE, value = TRUE
  )

  attributes(empgroupmeans)[datattributes] <- attributes(vismodeldata)[datattributes]

  emparg <- arg0
  emparg$modeldata <- empgroupmeans

  empcd <- do.call(coldist, emparg)

  empdS <- setNames(empcd$dS, paste(empcd$patch1, empcd$patch2, sep = "-"))


  # separate data by group
  bygroup <- lapply(unique(by), function(x) vismodeldata[by == x, ])

  # split(dat, by) also works but is about twice as slow
  names(bygroup) <- unique(by)

  # create vectors of indices to sample
  its <- lapply(samplesizes, function(x) sample(seq_len(x), x * boot.n, replace = TRUE))

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
    do.call(rbind, lapply(z, function(x) apply(x, 2, gmean)))
  })

  # now "split and merge"
  # creating a list with length = number of bootstrap replicates
  # and rows in each entry = mean per group in that replicate
  bootgrouped <- lapply(seq_len(boot.n), function(x) {
    do.call(rbind, lapply(groupcolmeans, "[", x, ))
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

  tmpbootcdfoo <- function(x) {
    tmparg <- arg0
    tmparg$modeldata <- x
    do.call(coldist, tmparg)
  }
  
  with_progress({
    p <- progressor(along = bootgrouped)
    bootcd <- future_lapply(bootgrouped, function(z) {
      p()
      tryCatch(suppressMessages(tmpbootcdfoo(z)),
               error = function(e) NULL
      )
    })
  })

  # get deltaS and name by group difference
  bootdS <- do.call(
    rbind,
    lapply(bootcd, function(x) {
      setNames(x$dS, paste(x$patch1, x$patch2, sep = "-"))
    })
  )

  if (dim(bootdS)[1] < boot.n) {
    stop('Bootstrap sampling encountered errors.')
  }
  # ...subtract them from the empirical
  # bootdS <- bootdS - empdS

  # ... sort and find quantiles
  # bootdS <- sort(bootdS)
  # quantileindices <- round(length(bootdS)*((1+c(-alpha, alpha))/2))
  # dsCI <- empdS + bootdS[quantileindices]

  # which gives the same as just
  quantileindices <- round(boot.n * ((1 + c(-alpha, alpha)) / 2))
  bootdS <- apply(bootdS, 2, sort)
  dsCI <- bootdS[quantileindices, , drop = FALSE]
  rownames(dsCI) <- c("dS.lwr", "dS.upr")
  # make sure names match with empirical (they always should but just in case)
  dsCI <- dsCI[, names(empdS), drop = FALSE]

  dS.mean <- empdS

  res <- t(rbind(dS.mean, dsCI))

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
    # make sure names match with empirical (they always should but just in case)
    dlCI <- dlCI[, names(empdL), drop = FALSE]
    dL.mean <- empdL
    res <- cbind(res, t(rbind(dL.mean, dlCI)))
  }

  res
}
