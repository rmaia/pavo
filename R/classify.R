#' Identify colour classes in an image for adjacency analyses
#'
#' Use k-means clustering to classify image pixels into discrete colour classes.
#'
#' @param imgdat (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param n_cols (required) either an integer, or vector the same length as imgdat (if
#' passing a list of images), specifying the number of discrete colour classes present
#' in an image, for k-means clustering.
#' @param ref_ID The numeric identifier of the 'reference' image, for use when passing
#' a list of images. Other images will be k-means classified using centres identified
#' in the reference image, thus helping to ensure that homologous pattern elements
#' will be reliably classified between images. Defaults to the first image in the list.
#' Ignored if n_cols is a vector.
#' @param manual manually specify the colour-category 'centers', for k-means clustering.
#' When \code{TRUE}, the user is asked to click a number of points (equal to \code{n_cols})
#' on the reference image that represent the distinct colours of interest.
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

classify <- function(imgdat, n_cols, ref_ID = 1, manual = FALSE, cores = getOption("mc.cores", 2L)) {

  ## Checks
  # Single or multiple images?
  multi_image <- inherits(imgdat, "list")
  # Cores
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
    message('Parallel processing not available in Windows; "cores" set to 1\n')
  }

  if (isTRUE(multi_image)) { # Multiple images
    if (length(n_cols) == length(imgdat)) { # Multiple k's TODO
      outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], n_cols[[x]]))
    } else if (length(n_cols) == 1 & manual == FALSE) { # Single k with reference
      ref_centers <- attr(classify_main(imgdat[[ref_ID]], n_cols), "classRGB") # k means centers of ref image
      if (format(object.size(imgdat), units = "Gb") < 0.5) {
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores)
      } else {
        message("Image data too large for parallel-processing, reverting to single-core processing.")
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers))
      }
    } else if (length(n_cols) == 1 & manual == TRUE) { # Single k with manual reference

      # Reference image
      refimg <- imgdat[[ref_ID]]

      cat(paste("Select the", n_cols, "focal colours."))

      plot(c(1, dim(refimg)[1]), c(1, dim(refimg)[2]), type = "n", xlab = "x", ylab = "y", asp = dim(refimg)[1] / dim(refimg)[2])
      rasterImage(refimg, 1, 1, dim(refimg)[1], dim(refimg)[2])
      reference <- as.data.frame(locator(type = "p", col = "red", n = n_cols))

      ref_centers <- as.data.frame(t(refimg[reference$x[1], reference$y[1], 1:3]))
      for (i in 2:n_cols)
        ref_centers <- rbind(ref_centers, refimg[reference$x[i], reference$y[i], 1:3])
      names(ref_centers) <- c("R", "G", "B")

      if (format(object.size(imgdat), units = "Gb") < 0.5) {
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores)
      } else {
        message("Image data too large for parallel-processing, reverting to single-core processing.")
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers))
      }
    }
    # Names & attributes
    for (i in 1:length(outdata)) {
      attr(outdata[[i]], "imgname") <- attr(imgdat[[i]], "imgname")
      attr(outdata[[i]], "k") <- n_cols ## what if multiple?
      attr(outdata[[i]], "state") <- "colclass"
    }
    class(outdata) <- c("rimg", "list")
  } else if (!isTRUE(multi_image)) { # Single image
    if (manual == TRUE) {
      # Reference (only) image
      refimg <- imgdat

      cat(paste("Select the", n_cols, "focal colours."))

      plot(c(1, dim(refimg)[1]), c(1, dim(refimg)[2]), type = "n", xlab = "x", ylab = "y")
      rasterImage(refimg, 1, 1, dim(refimg)[1], dim(refimg)[2])
      reference <- as.data.frame(locator(type = "p", col = "red", n = n_cols))

      ref_centers <- as.data.frame(t(refimg[reference$x[1], reference$y[1], 1:3]))
      for (i in 2:n_cols)
        ref_centers <- rbind(ref_centers, refimg[reference$x[i], reference$y[i], 1:3])
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
