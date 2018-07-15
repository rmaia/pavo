#' Identify colour classes in an image for adjacency analyses
#'
#' Use k-means clustering to classify image pixels into discrete colour classes.
#'
#' @param imgdat (required) image data. Either a single image, or a series of images
#' stored in a list. preferably the result of \code{\link{getimg}}.
#' @param kcols the number of discrete colour classes present in the input image(s).
#' Can be an integer when only a single image is present, or if kcols is identical for all
#' images. When passing a list of images, \code{kcols} can also be a vector the same length
#' as \code{imgdat}, or a data.frame with two columns specifying image file names and
#' corresponding kcols. Can be optionally disregarded when \code{interactive = TRUE},
#' and kcols will be inferred from the number of selections.
#' @param refID the optional numeric index of a 'reference' image, for use when passing
#' a list of images. Other images will be k-means classified using centres identified
#' in the single reference image, thus helping to ensure that homologous pattern elements
#' will be reliably classified between images, if so desired.
#' @param interactive interactively specify the colour-category 'centers', for k-means clustering.
#' When \code{TRUE}, the user is asked to click a number of points (equal to \code{kcols},
#' if specified, otherwise user-determined) that represent the distinct colours of interest.
#' If a reference image is specified, it will be the only image presented.
#' @param plotnew Should plots be opened in a new window when \code{interactive = TRUE}?
#' Defaults to \code{FALSE}.
#' @param cores number of cores to be used in parallel processing. If \code{1}, parallel
#'  computing will not be used. Defaults to \code{getOption("mc.cores", 2L)}.
#' @param ... additional graphical parameters when \code{interactive = TRUE}.
#' Also see \code{\link{par}}.
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
#' @importFrom tools file_path_sans_ext
#'
#' @note Since the \code{kmeans} process draws on random numbers to find initial
#' cluster centres when \code{interactive = FALSE}, use \code{set.seed} if reproducible
#' cluster ID's are desired between runs.
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, kcols = 4)
#'
#' # Multiple images, with interactive classification and a reference image
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' snakes_class <- classify(snakes, refID = 1, interactive = TRUE)
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

classify <- function(imgdat, kcols = NULL, refID = NULL, interactive = FALSE,
                     plotnew = FALSE, cores = getOption("mc.cores", 2L), ...) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Single or multiple images?
  multi_image <- inherits(imgdat, "list")

  ## Class/structure
  if (!multi_image) {
    if (!"rimg" %in% class(imgdat)) {
      message("Image is not of class 'rimg'; attempting to coerce.")
      imgdat <- as.rimg(imgdat)
    }
  } else if (multi_image) {
    if (any(unlist(lapply(1:length(imgdat), function(x) !"rimg" %in% class(imgdat[[x]]))))) {
      message("One or more images are not of class 'rimg'; attempting to coerce.")
      imgdat <- lapply(1:length(imgdat), function(x) as.rimg(imgdat[[x]]))
    }
  }

  ## Cores
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
    message('Parallel processing not available in Windows; "cores" set to 1\n')
  }

  ## k structure
  if (!is.null(kcols)) {

    # If kcols is a 2-col data frame/matrix
    if (!is.vector(kcols)) {

      # TODO more safety
      if (ncol(kcols) > 2) {
        warning("More than two columns included in kcols. Taking the first two columns only.")
        kcols <- as.data.frame(kcols[, 1:2])
      }

      # Identify the name of the column containing file names
      id_col <- names(kcols[lapply(kcols, class) != "numeric"])

      # Remove file extensions if present
      kcols[[id_col]] <- file_path_sans_ext(kcols[[id_col]])

      # Extract image names from image data
      imageIDs <- data.frame(
        names = unlist(lapply(
          1:length(imgdat),
          function(x) attr(imgdat[[x]], "imgname")
        )),
        stringsAsFactors = FALSE
      )

      # Reorder user-supplied kcols to match order of images
      kcols <- kcols[match(imageIDs[, 1], kcols[[id_col]]), ]

      # Extract kcols
      kcols <- as.numeric(unlist(kcols[lapply(kcols, class) == "numeric"]))
    }
    if (length(kcols) > 1) {
      # Must have k's for each image
      if (length(kcols) < length(imgdat)) {
        stop("When supplying more than one value, the length of kcols must equal the number of images.")
      }
      # Reduce to single integer if multiple k's are all the same
      if (length(unique(kcols)) == 1) {
        kcols <- kcols[1]
      }
      # Can't have a reference image when k's vary
      if (length(unique(kcols)) > 1 && !is.null(refID)) {
        message("Cannot use reference image when kcols varies between images. Ignoring refID.")
        refID <- NULL
      }
    }
  }
  
  ## ------------------------------ Main ------------------------------ ##
  
  #### So your options/configurations for classification are:
  #
  ## Multiple images ##
  # (1) Multiple different k's, no reference image (note: cannot have reference image - controlled above).
  #       (length(kcols) > 1 && interactive == FALSE)
  # (2) Single k (or multiple identical k), with a reference image.
  #       (length(kcols) == 1 && !is.null(refID) && interactive = FALSE)
  # (3) Single k (or multiple identical k), without a reference image, so the centres & assignments will vary between images.
  #       (length(kcols) == 1 && is.null(refID) && interactive = FALSE)
  # (4) Single or identical k (don't need to be pre-specified when interactive), with interactively-specified centres, and a single reference image.
  #       (!is.null(refID) && interactive == TRUE)
  # (5) Multiple k (identical or not, don't need to be specified), with interactively-specified centres for each image.
  #       (is.null(refID) && interactive == TRUE)
  #
  ## Single image ##
  # (1) Single k
  #      (length(kcols) == 1)
  # (2) Single k, with interactive centre
  #      (interactive == TRUE)

  ## Multiple images  ##
  if (multi_image) {
    imgsize <- format(object.size(imgdat), units = "Mb")

    ## (1) Multiple k, no reference image ##
    if (length(kcols) > 1 && interactive == FALSE) {
      ifelse(imgsize < 100,
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], kcols[[x]]), mc.cores = cores),
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], kcols[[x]]))
      )

      ## (2) Single k, with reference image ##
    } else if (length(kcols) == 1 && !is.null(refID) && interactive == FALSE) {
      ref_centers <- attr(classify_main(imgdat[[refID]], kcols), "classRGB") # k means centers of ref image
      ifelse(imgsize < 100,
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores),
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers))
      )

      ## (3) Single k, no reference image ##
    } else if (length(kcols) == 1 && is.null(refID) && interactive == FALSE) {
      ifelse(imgsize < 100,
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], kcols), mc.cores = cores),
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], kcols))
      )

      ## (4) Single k, interactively specified centre, with reference image ##
    } else if (!is.null(refID) && interactive == TRUE) {

      # Reference image
      refimg <- imgdat[[refID]]

      # Transformed image data (TODO: SIMPLIFY)
      reftrans <- array(c(
        as.matrix(t(apply(refimg[, , 1], 2, rev))),
        as.matrix(t(apply(refimg[, , 2], 2, rev))),
        as.matrix(t(apply(refimg[, , 3], 2, rev)))
      ),
      dim = c(
        dim(as.matrix(t(apply(refimg[, , 1], 2, rev))))[1],
        dim(as.matrix(t(apply(refimg[, , 1], 2, rev))))[2],
        3
      )
      )

      if (plotnew) dev.new(noRStudioGD = TRUE)

      defaultrasterImageplot(refimg, ...)

      if (!is.null(kcols)) {
        message(paste("Select the", kcols, "focal colours"))
        reference <- as.data.frame(locator(type = "p", col = "red", n = kcols))
      } else if (is.null(kcols)) {
        message(paste0("Select the focal colours in image ", attr(refimg, "imgname"), ", and press [esc] to continue."))
        reference <- as.data.frame(locator(type = "p", col = "red"))
        kcols <- nrow(reference)
      }

      if (plotnew) dev.off()

      ref_centers <- do.call(rbind, lapply(1:nrow(reference), function(x) as.data.frame(t(reftrans[reference$x[x], reference$y[x], 1:3]))))
      names(ref_centers) <- c("R", "G", "B")

      ifelse(imgsize < 100,
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers), mc.cores = cores),
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], ref_centers))
      )

      ## (5) Multiple k, with interactively-specified centres for each image. ##
    } else if (is.null(refID) && interactive == TRUE) {
      if (length(kcols) == 1) {
        kcols <- rep(kcols, length(imgdat))
      }
      if (is.null(kcols)) {
        n_cols_test <- NULL
        kcols <- rep(NA, length(imgdat))
      } else {
        n_cols_test <- FALSE
      }

      centers <- list()
      i <- 1
      while (i <= length(imgdat)) {

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

        if (plotnew) dev.new(noRStudioGD = TRUE)

        defaultrasterImageplot(imgdat[[i]], ...)

        if (!is.null(n_cols_test)) {
          message(paste0("Select the ", kcols[[i]], " focal colours in image ", attr(imgdat[[i]], "imgname", ".")))
          reference <- as.data.frame(locator(type = "p", col = "red", n = kcols[[i]]))
        } else if (is.null(n_cols_test)) {
          message(paste0("Select the focal colours in image ", attr(imgdat[[i]], "imgname"), ", and press [esc] to continue."))
          reference <- as.data.frame(locator(type = "p", col = "red"))
          kcols[[i]] <- nrow(reference)
        }
        if (plotnew) dev.off()

        ref_centers <- try(do.call(rbind, lapply(
          1:nrow(reference),
          function(x) as.data.frame(t(reftrans[reference$x[x], reference$y[x], 1:3]))
        )),
        silent = TRUE
        )
        centers[[i]] <- ref_centers

        if (class(centers[[i]]) == "try-error") {
          message("One or more coorodinates out-of bounds. Try again.")
          i <- i
        } else if (any(duplicated(centers[[i]]))) {
          message("Duplicate colours specified. Try again.")
          i <- i
        } else {
          names(centers[[i]]) <- c("R", "G", "B")
          i <- i + 1
        }
      }
      ifelse(imgsize < 100,
        outdata <- pbmclapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], centers[[x]]), mc.cores = cores),
        outdata <- lapply(1:length(imgdat), function(x) classify_main(imgdat[[x]], centers[[x]]))
      )
    }

    # Names & attributes
    for (i in 1:length(outdata)) {
      attr(outdata[[i]], "imgname") <- attr(imgdat[[i]], "imgname")
      attr(outdata[[i]], "outline") <- attr(imgdat[[i]], "outline")
      attr(outdata[[i]], "px_scale") <- attr(imgdat[[i]], "px_scale")
      attr(outdata[[i]], "raw_scale") <- attr(imgdat[[i]], "raw_scale")
      attr(outdata[[i]], "state") <- "colclass"
      if (length(kcols) > 1) {
        attr(outdata[[i]], "k") <- kcols[[i]]
      } else {
        attr(outdata[[i]], "k") <- kcols
      }
    }
    class(outdata) <- c("rimg", "list")
  }

  ## Single image ##
  if (!multi_image) {
    if (interactive == TRUE) {
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

      i <- 1
      while (i <= 1) {
        if (plotnew) dev.new(noRStudioGD = TRUE)

        defaultrasterImageplot(refimg, ...)

        if (!is.null(kcols)) {
          message(paste("Select the", kcols, "focal colours."))
          reference <- as.data.frame(locator(type = "p", col = "red", n = kcols))
        } else if (is.null(kcols)) {
          message(paste("Select the focal colours, and press [esc] to continue."))
          reference <- as.data.frame(locator(type = "p", col = "red"))
        }
        if (plotnew) dev.off()

        ref_centers <- try(do.call(rbind, lapply(
          1:nrow(reference),
          function(x) as.data.frame(t(reftrans[reference$x[x], reference$y[x], 1:3]))
        )),
        silent = TRUE
        )

        # Error controls
        if (class(ref_centers) == "try-error") {
          message("One or more coorodinates out-of bounds. Try again.")
          i <- i
        } else if (any(duplicated(ref_centers))) {
          message("Duplicate colours specified. Try again.")
          i <- i
        } else {
          names(ref_centers) <- c("R", "G", "B")
          i <- i + 1
        }
      }

      outdata <- classify_main(imgdat, ref_centers)
    } else {
      outdata <- classify_main(imgdat, kcols)
    }
    if (!is.null(kcols)) {
      attr(outdata, "k") <- kcols
    } else if (is.null(kcols)) {
      attr(outdata, "k") <- nrow(reference)
    }
    attr(outdata, "imgname") <- attr(imgdat, "imgname")
    attr(outdata, "outline") <- attr(imgdat, "outline")
    attr(outdata, "px_scale") <- attr(imgdat, "px_scale")
    attr(outdata, "raw_scale") <- attr(imgdat, "raw_scale")
    attr(outdata, "state") <- "colclass"
  }

  outdata
}

# Main function for identifying colour classes in an image for adjacency analyses
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
  attr(outmat, "colnames") <- data.frame(name = paste0('clr', 1:nrow(kMeans$centers)), stringsAsFactors = FALSE)

  outmat
}
