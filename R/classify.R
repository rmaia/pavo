#' Identify colour classes in an image for adjacency analyses
#'
#' Use k-means clustering to classify image pixels into discrete colour classes.
#'
#' @param imgdat (required) image data. Either a single image, or a series of images
#' stored in a list. Preferably the result of \code{\link{getimg}}.
#' @param kcols the number of discrete colour classes present in the input image(s).
#' Can be a single integer when only a single image is present, or if kcols is identical for all
#' images. When passing a list of images, \code{kcols} can also be a vector the same length
#' as \code{imgdat}, or a data.frame with two columns specifying image file names and
#' corresponding kcols. This argument can optionally be disregarded when \code{interactive = TRUE},
#' and kcols will be inferred from the number of selections.
#' @param refID either the numeric index or name of a 'reference' image, for use when passing
#' a list of images. Other images will be k-means classified using centres identified
#' in the single reference image, thus helping to ensure that homologous pattern elements
#' will be reliably classified between images, if so desired.
#' @param interactive interactively specify the colour-category 'centers', for k-means clustering.
#' When \code{TRUE}, the user is asked to click a number of points (equal to \code{kcols},
#' if specified, otherwise user-determined) that represent the distinct colours of interest.
#' If a reference image is specified, it will be the only image presented.
#' @param col the color of the marker points, when \code{interactive = TRUE}.
#' @param plotnew Should plots be opened in a new window when \code{interactive = TRUE}?
#' Defaults to \code{FALSE}.
#' @param cores number of cores to be used in parallel processing. If \code{1}, parallel
#'  computing will not be used. Defaults to \code{getOption("mc.cores", 2L)}. Not
#'  available on Windows.
#' @param ... additional graphical parameters when \code{interactive = TRUE}.
#' Also see \code{\link{par}}.
#'
#' @return A matrix, or list of matrices, of class \code{rimg} containing the colour
#' class classifications ID at each pixel location. The RGB values corresponding to
#' k-means centres (i.e. colour classes) are stored as object attributes.
#'
#' @export
#'
#' @importFrom stats kmeans
#' @importFrom utils object.size
#' @importFrom grDevices dev.new
#'
#' @note Since the \code{kmeans} process draws on random numbers to find initial
#' cluster centres when \code{interactive = FALSE}, use \code{set.seed} if reproducible
#' cluster ID's are desired between runs.
#'
#' @examples
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#' papilio_class <- classify(papilio, kcols = 4)
#'
#' # Multiple images, with interactive classification and a reference image
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' #snakes_class <- classify(snakes, refID = "snake_01", interactive = TRUE)
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

classify <- function(imgdat, kcols = NULL, refID = NULL, interactive = FALSE,
                     plotnew = FALSE, col = "red", cores = getOption("mc.cores", 2L), ...) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Single or multiple images?
  multi_image <- inherits(imgdat, "list")

  ## Convert refID to numeric identifier
  if(!is.null(refID)){
if (is.character(refID)) {
      refID <- which(unlist(lapply(seq_along(imgdat), function(x) attr(imgdat[[x]],"imgname"))) == refID)
      if(length(refID) == 0)
        stop("No image found with that name, specify another reference image using refID.")
    }
  }

  ## If it's a single image, store it in a list for processing convenience,
  ## before converting it back at the end
  if (!multi_image) {
    imgdat <- list(imgdat)
  }

  # Need options
  if (!interactive && is.null(kcols)) {
    stop("Either kcols must be specified, or interactive classification used (via interactive = TRUE)")
  }

  ## Class/structure
    if (!all(unlist(lapply(imgdat, is.rimg)))) {
      message("One or more images are not of class 'rimg'; attempting to coerce.")
      imgdat <- lapply(imgdat, as.rimg)
    }

  ## Cores
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
  }

  ## kcols
  if (!is.null(kcols)) {
    # Can't have a reference image when k's vary
    if (length(unique(kcols)) > 1 && !is.null(refID)) {
      message("Cannot use reference image when kcols varies between images. Ignoring refID.")
      refID <- NULL
    }
    kcols <- parse_kcols(kcols, imgdat)
  }

  ## ------------------------------ Main ------------------------------ ##

  #### So your options/configurations for classification are:

  # (1) Non-interactive, no reference image.
  # (2) Non-interactive, with a reference image.
  # (3) Interactive, no reference image.
  # (4) Interctive, with a reference image

  # Image size check to avoid pbmc bug when faced with large objects
  if (format(object.size(imgdat), units = "Mb") < 100) {
    parallel <- TRUE
  } else {
    parallel <- FALSE
  }

  if (!interactive) {
    if (!is.null(refID)) { ## (2) Single k, with reference image ##
      ref_centers <- attr(classify_main(imgdat[[refID]], kcols[[refID]]), "classRGB")
      ref_centers <- rep(list(ref_centers), length(imgdat))
    }
    else { ## (1) Non-interactive, with a reference image. ##
      ref_centers <- kcols
    }

    # Classify
    message("Image classification in progress...")
    outdata <- classifier(imgdat, ref_centers, parallel, cores)
  } else if (interactive) {

    ## (3) Interactive, no reference image. ##
    if (is.null(kcols)) {
      kcols <- rep(list(512), length(imgdat))
    }

    centers <- list()
    tag_loc <- list()
    if (!is.null(refID)) {
      i <- refID
    } else {
      i <- 1
    }

    while (i <= length(imgdat)) {
      if (plotnew) dev.new(noRStudioGD = TRUE)

      plot(imgdat[[i]], ...)

      message(paste0(
        "Select the focal colours in image ",
        attr(imgdat[[i]], "imgname"), ", and press [esc] to continue."
      ))
      reference <- as.data.frame(locator(type = "p", col = col, n = kcols[[i]]))
      kcols[[i]] <- nrow(reference)

      if (plotnew) dev.off()

      ref_centers <- try(do.call(rbind, lapply(
        seq_len(nrow(reference)),
        function(x) as.data.frame(t(imgdat[[i]][reference$x[x], reference$y[x], 1:3]))
      )),
      silent = TRUE
      )
      centers[[i]] <- ref_centers
      tag_loc[[i]] <- reference

      # Error prevention
      if (class(centers[[i]]) == "try-error") {
        message("One or more coorodinates out-of bounds. Try again.")
        i <- i
      } else if (any(duplicated(centers[[i]]))) {
        message("Duplicate colours specified. Try again.")
        i <- i
      } else if (!is.null(refID)) {
        ## (4) Interctive, with a reference image. ##
        names(centers[[i]]) <- c("R", "G", "B")
        centers <- rep(list(centers[[i]]), length(imgdat))
        tag_loc <- rep(list(tag_loc[[i]]), length(imgdat))
        i <- length(imgdat) + 1
      } else {
        names(centers[[i]]) <- c("R", "G", "B")
        i <- i + 1
      }
    }

    # Classify
    message("Image classification in progress...")
    outdata <- classifier(imgdat, centers, parallel, cores)
  }

  # Names & attributes
  for (i in seq_along(outdata)) {
    attr(outdata[[i]], "imgname") <- attr(imgdat[[i]], "imgname")
    attr(outdata[[i]], "outline") <- attr(imgdat[[i]], "outline")
    attr(outdata[[i]], "px_scale") <- attr(imgdat[[i]], "px_scale")
    attr(outdata[[i]], "raw_scale") <- attr(imgdat[[i]], "raw_scale")
    attr(outdata[[i]], "state") <- "colclass"
    if (interactive) {
      if (!is.null(refID)) {
        attr(outdata[[refID]], "tag_loc") <- tag_loc
      } else {
        attr(outdata[[i]], "tag_loc") <- tag_loc[[i]]
      }
    } else {
      attr(outdata[[i]], "tag_loc") <- NA
    }
    if (length(kcols) > 1) {
      attr(outdata[[i]], "k") <- kcols[[i]]
    } else {
      attr(outdata[[i]], "k") <- kcols
    }
  }

  if (multi_image) {
    class(outdata) <- c("rimg", "list")
  } else {
    outdata <- outdata[[1]]
  }
  outdata
}

# Wrapper for main classify function to handle parallel/single core
#' @importFrom pbmcapply pbmclapply
classifier <- function(imgdat_i2, n_cols_i2, parallel_i2, cores_i2) {
  ifelse(parallel_i2,
    outdata <- pbmclapply(seq_along(imgdat_i2),
      function(x) classify_main(imgdat_i2[[x]], n_cols_i2[[x]]),
      mc.cores = cores_i2
    ),
    outdata <- lapply(seq_along(imgdat_i2), function(x) classify_main(imgdat_i2[[x]], n_cols_i2[[x]]))
  )
  outdata
}

# Main function for identifying colour classes in an image for adjacency analyses
classify_main <- function(imgdat_i, n_cols_i) {

  ## Dimensions
  imgdim <- dim(imgdat_i)

  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    R = as.vector(imgdat_i[, , 1]),
    G = as.vector(imgdat_i[, , 2]),
    B = as.vector(imgdat_i[, , 3])
  )

  # Cluster analysis
  kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = n_cols_i)

  # Tidy & format as image matrix
  outmat3 <- matrix(kMeans$cluster, nrow = imgdim[1])

  # Rotate to match original orientation
  outmat2 <- rev(t(apply(outmat3, 1, rev))) # mirror
  dim(outmat2) <- dim(outmat3)
  outmat <- t(apply(outmat2, 2, rev)) # rotate 90

  # Attributes
  class(outmat) <- c("rimg", "matrix")
  attr(outmat, "classRGB") <- as.data.frame(kMeans$centers)
  # attr(outmat, "colnames") <- data.frame(name = paste0("clr", 1:nrow(kMeans$centers)), stringsAsFactors = FALSE)
  attr(outmat, "colnames") <- data.frame(name = seq_len(nrow(kMeans$centers)))
  attr(outmat, "tag_loc") <- NA

  outmat
}

## k structure parser
#' @importFrom tools file_path_sans_ext
parse_kcols <- function(kcols_i, imgdat_i) {

  # If kcols is a 2-col data frame/matrix
  if (!is.vector(kcols_i)) {

    # TODO more safety
    if (ncol(kcols_i) > 2) {
      warning("More than two columns included in kcols. Taking the first two columns only.")
      kcols_i <- as.data.frame(kcols_i[, 1:2])
    }

    # Identify the name of the column containing file names
    id_col <- names(kcols_i[lapply(kcols_i, class) != "numeric"])

    # Remove file extensions if present
    kcols_i[[id_col]] <- file_path_sans_ext(kcols_i[[id_col]])

    # Extract image names from image data
    imageIDs <- data.frame(
      names = unlist(lapply(
        imgdat_i,
        function(x) attr(x, "imgname")
      )),
      stringsAsFactors = FALSE
    )

    # Reorder user-supplied kcols to match order of images
    kcols_i <- kcols_i[match(imageIDs[, 1], kcols_i[[id_col]]), ]

    # Extract kcols
    kcols_i <- as.numeric(unlist(kcols_i[lapply(kcols_i, class) == "numeric"]))
  }
  if (length(kcols_i) > 1) {
    # Must have k's for each image
    if (length(kcols_i) < length(imgdat_i)) {
      stop("When supplying more than one value, the length of kcols must equal the number of images.")
    }
  }
  # Return a list of identical kcols if only one is supplied
  if (length(kcols_i) == 1 && length(imgdat_i) > 1) {
    kcols_i <- rep(kcols_i, length(imgdat_i))
  }

  kcols_i
}
