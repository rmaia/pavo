#' Identify colour classes in an image for adjacency analyses
#'
#' Classify image pixels into discrete colour classes.
#'
#' @param imgdat (required) image data. Either a single image, or a series of images
#' stored in a list. Preferably the result of [getimg()].
#' @param method methods for image segmentation/classification.
#' * `'kMeans'`: k-means clustering (default)
#' * `'kMedoids'`: k-medoids clustering, using the partitioning-around-medoids ('pam')
#' algorithm for large datasets.
#' @param kcols the number of discrete colour classes present in the input image(s).
#' Can be a single integer when only a single image is present, or if kcols is identical for all
#' images. When passing a list of images, `kcols` can also be a vector the same length
#' as `imgdat`, or a data.frame with two columns specifying image file names and
#' corresponding kcols. This argument can optionally be disregarded when `interactive = TRUE`,
#' and kcols will be inferred from the number of selections.
#' @param refID either the numeric index or name of a 'reference' image, for use when passing
#' a list of images. Other images will be k-means classified using centres identified
#' in the single reference image, thus helping to ensure that homologous pattern elements
#' will be reliably classified between images, if so desired.
#' @param interactive interactively specify the colour-category 'centers', for k-means clustering.
#' When `TRUE`, the user is asked to click a number of points (equal to `kcols`,
#' if specified, otherwise user-determined) that represent the distinct colours of interest.
#' If a reference image is specified, it will be the only image presented.
#' @param col the color of the marker points, when `interactive = TRUE`.
#' @param plotnew Should plots be opened in a new window when `interactive = TRUE`?
#' Defaults to `FALSE`.
#' @param ... additional graphical parameters when `interactive = TRUE`.
#' Also see [graphics::par()].
#' @inheritParams getspec
#' 
#' @inherit getspec details
#'
#' @return A matrix, or list of matrices, of class `rimg` containing the colour
#' class classifications ID at each pixel location. The RGB values corresponding to
#' cluster centres (i.e. colour classes) are stored as object attributes.
#'
#' @export
#'
#' @importFrom stats kmeans
#' @importFrom utils object.size
#' @importFrom grDevices dev.new
#'
#' @note Since the `kmeans` process draws on random numbers to find initial
#' cluster centres when `interactive = FALSE`, use [set.seed()] if reproducible
#' cluster ID's are desired between runs.
#'
#' @seealso [stats::kmeans]
#'
#' @examples
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
#' papilio_class <- classify(papilio, kcols = 4)
#'
#' # Multiple images, with interactive classification and a reference image
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' \dontrun{
#' snakes_class <- classify(snakes, refID = "snake_01", interactive = TRUE)
#' }
#' 
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

classify <- function(imgdat, method = c("kMeans", "kMedoids"), kcols = NULL, refID = NULL, interactive = FALSE,
                     plotnew = FALSE, col = "red", cores = NULL, ...) {

  if (!missing(cores)) {
    warning("'cores' argument is deprecated. See ?future::plan for more info ",
            "about how you can choose your parallelisation strategy.",
            call. = FALSE)
  }

  ## ------------------------------ Checks ------------------------------ ##

  ## Single or multiple images?
  multi_image <- inherits(imgdat, "list")

  ## If it's a single image, store it in a list for processing convenience,
  ## before converting it back at the end
  if (!multi_image) {
    imgdat <- list(imgdat)
  }

  ## Convert refID to numeric identifier
  if (!is.null(refID)) {
    if (is.character(refID)) {
      refID <- which(unlist(lapply(imgdat, function(x) attr(x, "imgname"))) == refID)
      if (length(refID) == 0) {
        stop("No image found with that name, specify another reference image using refID.")
      }
    }
  }

  # Need options
  if (!interactive && is.null(kcols)) {
    stop("Either kcols must be specified, or interactive classification used (via interactive = TRUE)")
  }

  # Method
  method2 <- tryCatch(
    match.arg(method),
    error = function(e) "kMeans"
  )

  # Cannot currently use pre-specified centres with k-medoids. Annoying.
  if (interactive && method2 == "kMedoids") {
    stop("Cannot currently interactively classify images using k-medoids, set kcols instead.")
  }
  if (!is.null(refID) && method2 == "kMedoids") {
    stop("Cannot currently use a reference image when using k-medoids")
  }

  ## Class/structure
  if (!all(unlist(lapply(imgdat, is.rimg)))) {
    message("One or more images are not of class 'rimg'; attempting to coerce.")
    imgdat <- lapply(imgdat, as.rimg)
  }

  ## kcols
  if (!is.null(kcols)) {
    # Can't have a reference image when k's vary
    if (length(unique(Filter(is.numeric, kcols))) > 1 && !is.null(refID)) {
      message("Cannot use reference image when kcols varies between images. Ignoring refID.")
      refID <- NULL
    }
    kcols <- parse_kcols(kcols, imgdat)
  }

  ## Check distinct data points > kcols, otherwise it'll give an uninformative error or hang.
  ## Doesn't work for interactive atm though
  if (!is.null(kcols)) {
    if (any(unlist(lapply(imgdat, function(x) nrow(unique(apply(x, 3, rbind))))) < max(kcols))) {
      stop('The specified number of cluster centers exceeds the number of distinct data points in one or more images, consider a new value for argument "kcols"')
    }
  }

  ## ------------------------------ Main ------------------------------ ##

  #### So your options/configurations for classification are:

  # (1) Non-interactive, no reference image.
  # (2) Non-interactive, with a reference image.
  # (3) Interactive, no reference image.
  # (4) Interctive, with a reference image

  if (!interactive) {
    if (!is.null(refID)) { ## (2) Single k, with reference image ##
      ref_centers <- attr(classify_main(imgdat[[refID]], kcols[[refID]], method2), "classRGB")
      ref_centers <- rep(list(ref_centers), length(imgdat))
    }
    else { ## (1) Non-interactive, with a reference image. ##
      ref_centers <- kcols
    }

    # Classify
    message("Image classification in progress...")
    outdata <- classifier(imgdat, ref_centers, method2)
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

      message(
        "Select the focal colours in image ", attr(imgdat[[i]], "imgname"),
        ", and press [esc] to continue."
      )
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
      if (inherits(centers[[i]], "try-error")) {
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
    outdata <- classifier(imgdat, centers, method2)
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
#' @importFrom future.apply future_lapply
#' @importFrom progressr with_progress progressor
classifier <- function(imgdat_i2, n_cols_i2, method_i2) {
  
  with_progress({
    p <- progressor(along = imgdat_i2)
    outdata <- future_lapply(seq_along(imgdat_i2), function(x) {
      p()
      classify_main(imgdat_i2[[x]], n_cols_i2[[x]], method_i2)
    })
  })

  outdata
}

# Main function for identifying colour classes in an image for adjacency analyses
#' @importFrom cluster clara
classify_main <- function(imgdat_i, n_cols_i, method_i) {

  ## Dimensions
  imgdim <- dim(imgdat_i)

  # Set minimum sample size for kMedoids
  samsize <- min(200, (imgdim[1] * imgdim[2]))

  # Assign RGB channels to data frame
  imgRGB <- data.frame(
    R = as.vector(imgdat_i[, , 1]),
    G = as.vector(imgdat_i[, , 2]),
    B = as.vector(imgdat_i[, , 3])
  )

  # Cluster analysis, then format as image matrix
  kMeans <- switch(method_i,
    "kMeans" = kmeans(imgRGB[, c("R", "G", "B")], centers = n_cols_i),
    "kMedoids" = clara(imgRGB[, c("R", "G", "B")], k = n_cols_i, samples = 100, sampsize = samsize, pamLike = TRUE)
  )
  outmat3 <- switch(method_i,
    "kMeans" = matrix(kMeans$cluster, nrow = imgdim[1]),
    "kMedoids" = matrix(kMeans$clustering, nrow = imgdim[1])
  )
  centers <- switch(method_i,
    "kMeans" = as.data.frame(kMeans$centers),
    "kMedoids" = as.data.frame(kMeans$medoids)
  )

  # Rotate to match original orientation
  outmat2 <- rev(t(apply(outmat3, 1, rev))) # mirror
  dim(outmat2) <- dim(outmat3)
  outmat <- t(apply(outmat2, 2, rev)) # rotate 90

  # Attributes
  class(outmat) <- c("rimg", "matrix")
  attr(outmat, "classRGB") <- centers
  # attr(outmat, "colnames") <- data.frame(name = paste0("clr", 1:nrow(kMeans$centers)), stringsAsFactors = FALSE)
  attr(outmat, "colnames") <- data.frame(name = seq_len(nrow(centers)))
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
    id_col <- names(kcols_i[!vapply(kcols_i, is.numeric, logical(1))])

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
    kcols_i <- as.numeric(unlist(kcols_i[vapply(kcols_i, is.numeric, logical(1))]))
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
