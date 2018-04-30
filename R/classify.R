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
#'
#' @return A matrix, or list of matrices, of class \code{rimg} containing the colour 
#' class classifications at each pixel location. The RGB values corresponding to 
#' k-means centres (i.e. colour classes) are stored as object attributes.
#'
#' @export
#'
#' @note Since the \code{kmeans} process draws on random numbers to find initial
#' cluster centres, use \code{set.seed} if reproducible cluster ID's are desired
#' between runs.
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

classify <- function(imgdat, n_cols, ref_ID = 1) {

  multi_image <- inherits(imgdat, "list")  # Single or multiple images?

  if (isTRUE(multi_image)) { # Multiple images
    if(length(n_cols) == length(imgdat)){  # Multiple k's
      outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], n_cols[[x]]))
    }else if(length(n_cols) == 1){  # Single k with reference
      ref_centers <- attr(classify_main(imgdat[[ref_ID]], n_cols), "classRGB") # k means centers of ref image
      outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers))
    }
    # Names & attributes
    for (i in 1:length(outdata)) {
      attr(outdata[[i]], "imgname") <- attr(imgdat[[i]], "imgname")
      attr(outdata[[i]], "k") <- n_cols                             ## what if multiple?
      attr(outdata[[i]], 'state') <- 'colclass'
    }
    class(outdata) <- c("rimg", "list")

  } else if (!isTRUE(multi_image)) { # Single image
    outdata <- classify_main(imgdat, n_cols)
    attr(outdata, "imgname") <- attr(imgdat, "imgname")
    attr(outdata, "k") <- n_cols
    attr(outdata, 'state') <- 'colclass'
  }

  if(!is.null(attr(imgdat, "px_scale")))
    attr(outdata, "px_scale") <- attr(imgdat, "px_scale")

  outdata
}

## Internal calcs
classify_main <- function(imagedata, n_colours) {

  ## Dimensions
  imgdim <- dim(imagedata)

  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    x = rep(imgdim[1]:1, imgdim[2]),
    y = rep(1:imgdim[2], each = imgdim[1]),
    R = as.vector(imagedata[, , 1]),
    G = as.vector(imagedata[, , 2]),
    B = as.vector(imagedata[, , 3])
  )

  # Cluster analysis
  kMeans <- stats::kmeans(imgRGB[, c("R", "G", "B")], centers = n_colours)

  # Tidy & format as image matrix
  cmbn <- cbind(imgRGB, kMeans$cluster)
  names(cmbn) <- c("x", "y", "ch1", "ch2", "ch3", "class")

  outmat2 <- as.data.frame.matrix(stats::xtabs(class ~ x + y, data = cmbn))

  # Rotate to match original orientation
  outmat <- rev(t(apply(outmat2, 1, rev)))
  dim(outmat) <- dim(outmat2)

  # Attributes
  class(outmat) <- c("rimg", "matrix")
  attr(outmat, "classRGB") <- as.data.frame(kMeans$centers)

  outmat
}
