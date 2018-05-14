#' Identify colour classes in an image for adjacency analyses
#'
#' Use k-means clustering to classify image pixels into discrete colour classes.
#'
#' @param imgdat (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param n_cols either an integer, or vector the same length as imgdat (if
#' passing a list of images), specifying the number of discrete colour classes present
#' in an image, for k-means clustering. Not required if n_cols has already been set via
#' \code{\link{calibrate}}, though will be overwritten by the values specified here.
#' @param ref_ID The numeric identifier of the 'reference' image, for use when passing
#' a list of images. Other images will be k-means classified using centres identified
#' in the reference image, thus helping to ensure that homologous pattern elements
#' will be reliably classified between images, if so desired. 
#' @param manual manually specify the colour-category 'centers', for k-means clustering.
#' When \code{TRUE}, the user is asked to click a number of points (equal to \code{n_cols})
#' that represent the distinct colours of interest. If a reference image is specified,
#' it will be the only image presented.
#' @param window Should plots be openened in a new window when \code{manual = TRUE}?
#' Defaults to \code{FALSE}.
#' @param cores number of cores to be used in parallel processing. If \code{1}, parallel
#'  computing will not be used. Defaults to \code{getOption("mc.cores", 2L)}.
#'
#' @return A matrix, or list of matrices, of class \code{rimg} containing the colour
#' class classifications at each pixel location. The RGB values corresponding to
#' k-means centres (i.e. colour classes) are stored as object attributes.
#'
#' @export
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom stats kmeans xtabs
#' @importFrom utils object.size
#' @importFrom grDevices dev.new
#'
#' @note Since the \code{kmeans} process draws on random numbers to find initial
#' cluster centres when \code{manual = FALSE}, use \code{set.seed} if reproducible
#' cluster ID's are desired between runs.
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, n_cols = 4)
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes_class <- classify(snakes, n_cols = 3)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

classify <- function(imgdat, n_cols, ref_ID = NULL, manual = FALSE, window = FALSE, cores = getOption("mc.cores", 2L)) {

  ## Checks
  # Single or multiple images?
  multi_image <- inherits(imgdat, "list")
  # Cores
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
    message('Parallel processing not available in Windows; "cores" set to 1\n')
  }
  # Extract n_cols if present as an object attribute following calibrate().
  if (isTRUE(multi_image)) {
    if (!is.null(attr(imgdat[[1]], "k"))) {
      if (!is.null(n_cols)) {
        warning("n_cols argument specified for already-calibrated images, using n_cols as specified.")
        n_cols <- n_cols
      } else {
        n_cols <- as.numeric(unlist(lapply(1:length(imgdat), function(x) attr(imgdat[[x]], "k"))))
      }
    }
  } else if (!isTRUE(multi_image)) {
    if (!is.null(attr(imgdat, "k"))) {
      n_cols <- attr(imgdat, "k")
    }
  }
  # k checking.
  if (length(n_cols) > 1) {
    # Must have k's for each image
    if (length(n_cols) < length(imgdat)) {
      stop("When supplying more than one value, the length of n_cols must equal the number of images.")
    }
    # Reduce to single integer if multiple k's are all the same
    if (length(unique(n_cols)) == 1) {
      n_cols <- n_cols[1]
    }
    # Can't have a reference image when k's vary
    if (length(unique(n_cols)) > 1 && !is.null(ref_ID)) {
      warning("Cannot use reference image when n_cols varies between images. Ignoring ref_ID.")
      ref_ID <- NULL
    }
  }

  #### So your options/configurations for classification are:
  #
  ## Multiple images ##
  # (1) Multiple different k's, no reference image (note: cannot have reference image - controlled above).
  #       (length(n_cols) > 1 && manual = FALSE)
  # (2) Single k (or multiple identical k), with a reference image.
  #       (length(n_cols) == 1 && !is.null(ref_ID) && manual = FALSE)
  # (3) Single k (or multiple identical k), without a reference image, so the centres & assignments will vary between images.
  #       (length(n_cols) == 1 && is.null(ref_ID) && manual = FALSE)
  # (4) Single k (or multiple identical k), with manually-specified centres, and a single reference image.
  #       (length(n_cols) == 1 && !is.null(ref_ID) && manual == TRUE)
  # (5) Multiple k (identical or not), with manually-specified centres for each image.
  #       (is.null(ref_ID) && manual == TRUE)
  #
  ## Single image ##
  # - Single k
  #   (length(n_cols) == 1)
  # - Single k, with manual centre
  #   (length(n_cols) == 1 && manual = TRUE)


  ## Multiple images  ##
  if (isTRUE(multi_image)) {

    ## Multiple k, no reference image ##
    if (length(n_cols) > 1 && manual == FALSE) {
      outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], n_cols[[x]]), mc.cores = cores)

      ## Single k, with reference image ##
    } else if (length(n_cols) == 1 && !is.null(ref_ID) && manual == FALSE) {
      ref_centers <- attr(classify_main(imgdat[[ref_ID]], n_cols), "classRGB") # k means centers of ref image
      outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores)

      ## Single k, no reference image ##
    } else if (length(n_cols) == 1 && is.null(ref_ID) && manual == FALSE) {
      outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], n_cols), mc.cores = cores)

      ## Single k, manually specified centres, with reference image ##
    } else if (length(n_cols) == 1 && !is.null(ref_ID) && manual == TRUE) {

      # Reference image
      refimg <- imgdat[[ref_ID]]

      message(paste("Select the", n_cols, "focal colours"))

      if (window == TRUE) {
        dev.new(width = 8, height = 8)
      }
      plot(c(1, dim(refimg)[1]), c(1, dim(refimg)[2]), type = "n", xlab = "x", ylab = "y", asp = dim(refimg)[1] / dim(refimg)[2])
      rasterImage(refimg, 1, 1, dim(refimg)[1], dim(refimg)[2])
      reference <- as.data.frame(locator(type = "p", col = "red", n = n_cols))
      dev.off()

      ref_centers <- as.data.frame(t(refimg[reference$x[1], reference$y[1], 1:3]))
      for (i in 2:n_cols)
        ref_centers <- rbind(ref_centers, refimg[reference$x[i], reference$y[i], 1:3])
      names(ref_centers) <- c("R", "G", "B")

      outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores)

      ## (5) Multiple k, with manually-specified centres for each image. ##
    } else if (is.null(ref_ID) && manual == TRUE) {
      if (length(n_cols) == 1) {
        n_cols <- rep(n_cols, length(imgdat))
      }

      centers <- list()
      i <- 1
      while (i <= length(imgdat)) {
        message(paste0("Select the ", n_cols[[i]], " focal colours in image ", attr(imgdat[[i]], "imgname", ".")))

        if (window == TRUE) {
          dev.new(width = 8, height = 8)
        }
        plot(c(1, dim(imgdat[[i]])[1]), c(1, dim(imgdat[[i]])[2]),
          type = "n",
          xlab = "x",
          ylab = "y",
          asp = dim(imgdat[[i]])[1] / dim(imgdat[[i]])[2]
        )
        rasterImage(imgdat[[i]], 1, 1, dim(imgdat[[i]])[1], dim(imgdat[[i]])[2])
        reference <- as.data.frame(locator(type = "p", col = "red", n = n_cols[[i]]))
        dev.off()

        # Transformed image data (TODO: SIMPLIFY)
        reftrans <- array(c(
          as.matrix(t(apply(imgdat[[i]][, , 1], 2, rev))),
          as.matrix(t(apply(imgdat[[i]][, , 2], 2, rev))),
          as.matrix(t(apply(imgdat[[i]][, , 3], 2, rev)))
        ),
        dim = c(
          dim(as.matrix(t(apply(imgdat[[i]][, , 1], 2, rev))))[1],
          dim(as.matrix(t(apply(imgdat[[i]][, , 1], 2, rev))))[2],
          3
        )
        )

        ref_centers <- as.data.frame(t(reftrans[reference$x[1], reference$y[1], 1:3]))
        for (j in 2:n_cols[[i]]) {
          ref_centers <- rbind(ref_centers, reftrans[reference$x[j], reference$y[j], 1:3])
        }
        names(ref_centers) <- c("R", "G", "B")
        centers[[i]] <- ref_centers

        # If duplicates centers specified, try again
        if (any(duplicated(centers[[i]]))) {
          message("Duplicate colours specified. Select again, or use [esc] to abort.")
          i <- i
        } else {
          i <- i + 1
        }
      }
      outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], centers[[x]]), mc.cores = cores)
    }

    # Names & attributes
    for (i in 1:length(outdata)) {
      attr(outdata[[i]], "imgname") <- attr(imgdat[[i]], "imgname")
      attr(outdata[[i]], "state") <- "colclass"
      if (length(n_cols) > 1) {
        attr(outdata[[i]], "k") <- n_cols[[i]]
      } else {
        attr(outdata[[i]], "k") <- n_cols
      }
    }
    class(outdata) <- c("rimg", "list")
  }

  ## Single image ##
  if (!isTRUE(multi_image)) {
    if (manual == TRUE) {
      # Reference (only present) image
      refimg <- imgdat

      # Transformed image data (TODO: SIMPLIFY)
      reftrans <- array(c(
        as.matrix(t(apply(imgdat[, , 1], 2, rev))),
        as.matrix(t(apply(imgdat[, , 2], 2, rev))),
        as.matrix(t(apply(imgdat[, , 3], 2, rev)))
      ),
      dim = c(
        dim(as.matrix(t(apply(imgdat[, , 1], 2, rev))))[1],
        dim(as.matrix(t(apply(imgdat[, , 1], 2, rev))))[2],
        3
      )
      )

      message(paste("Select the", n_cols, "focal colours."))

      if (window == TRUE) {
        dev.new(width = 8, height = 8)
      }
      plot(c(1, dim(refimg)[1]), c(1, dim(refimg)[2]), type = "n", xlab = "x", ylab = "y")
      rasterImage(refimg, 1, 1, dim(refimg)[1], dim(refimg)[2])
      reference <- as.data.frame(locator(type = "p", col = "red", n = n_cols))

      ref_centers <- as.data.frame(t(reftrans[reference$x[1], reference$y[1], 1:3]))
      for (i in 2:n_cols)
        ref_centers <- rbind(ref_centers, reftrans[reference$x[i], reference$y[i], 1:3])
      names(ref_centers) <- c("R", "G", "B")

      outdata <- classify_main(imgdat, ref_centers)
    } else {
      outdata <- classify_main(imgdat, n_cols)
    }
    attr(outdata, "imgname") <- attr(imgdat, "imgname")
    attr(outdata, "k") <- n_cols
    attr(outdata, "state") <- "colclass"
  }

  if (!is.null(attr(imgdat, "px_scale"))) {
    attr(outdata, "px_scale") <- attr(imgdat, "px_scale")
  }

  outdata
}

#' Main function for identifying colour classes in an image for adjacency analyses
#'
#' @param imgdat_i (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param n_cols_i (required) either an integer, or vector the same length as imgdat (if
#' passing a list of images), specifying the number of discrete colour classes present
#' in an image, for k-means clustering.
#'
#' @keywords internal
#'
#' @return A matrix, or list of matrices, of class \code{rimg} containing the colour
#' class classifications at each pixel location. The RGB values corresponding to
#' k-means centres (i.e. colour classes) are stored as object attributes.
#'
classify_main <- function(imgdat_i, n_cols_i) {

  ## Dimensions
  imgdim <- dim(imgdat_i)

  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    x = rep(imgdim[1]:1, imgdim[2]),
    y = rep(1:imgdim[2], each = imgdim[1]),
    R = as.vector(imgdat_i[, , 1]),
    G = as.vector(imgdat_i[, , 2]),
    B = as.vector(imgdat_i[, , 3])
  )

  # Cluster analysis
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = n_cols_i)

  # Tidy & format as image matrix
  cmbn <- cbind(imgRGB, kMeans$cluster)
  names(cmbn) <- c("x", "y", "ch1", "ch2", "ch3", "class")

  outmat2 <- as.data.frame.matrix(xtabs(class ~ x + y, data = cmbn))

  # Rotate to match original orientation
  outmat <- rev(t(apply(outmat2, 1, rev)))
  dim(outmat) <- dim(outmat2)

  # Attributes
  class(outmat) <- c("rimg", "matrix")
  attr(outmat, "classRGB") <- as.data.frame(kMeans$centers)

  outmat
}
