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
#' @param refID the optional numeric index of a 'reference' image, for use when passing
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
#' @importFrom pbmcapply pbmclapply
#' @importFrom stats kmeans
#' @importFrom utils object.size
#' @importFrom grDevices dev.new
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
                     plotnew = FALSE, col = "red", cores = getOption("mc.cores", 2L), ...) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Single or multiple images?
  multi_image <- inherits(imgdat, "list")

  # Need options
  if (!interactive && is.null(kcols)) {
    stop("Either kcols must be specified, or interactive classification used (via interactive = TRUE)")
  }

  ## Class/structure
  if (!multi_image) {
    if (!"rimg" %in% class(imgdat)) {
      message("Image is not of class 'rimg'; attempting to coerce.")
      imgdat <- as.rimg(imgdat)
    }
  } else {
    if (any(unlist(lapply(imgdat, function(x) !"rimg" %in% class(x))))) {
      message("One or more images are not of class 'rimg'; attempting to coerce.")
      imgdat <- lapply(imgdat, function(x) as.rimg(x))
    }
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

  ## If it's a single image, store it in a list for processing convenience,
  ## before converting it back at the end
  if (!multi_image) {
    imgdat <- list(imgdat)
  }

  ## ------------------------------ Main ------------------------------ ##

  #### So your options/configurations for classification are:

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

  imgsize <- format(object.size(imgdat), units = "Mb")

  ## (1) Multiple k, no reference image ##
  if (length(kcols) > 1 && interactive == FALSE) {
    message("Image classification in progress...")
    ifelse(imgsize < 100,
      outdata <- pbmclapply(seq_along(imgdat),
        function(x) classify_main(imgdat[[x]], kcols[[x]]),
        mc.cores = cores
      ),
      outdata <- lapply(
        seq_along(imgdat),
        function(x) classify_main(imgdat[[x]], kcols[[x]])
      )
    )

    ## (2) Single k, with reference image ##
  } else if (length(kcols) == 1 && !is.null(refID) && interactive == FALSE) {
    ref_centers <- attr(classify_main(imgdat[[refID]], kcols), "classRGB") # k means centers of ref image
    message("Image classification in progress...")
    ifelse(imgsize < 100,
      outdata <- pbmclapply(imgdat, function(x) classify_main(x, ref_centers), mc.cores = cores),
      outdata <- lapply(imgdat, function(x) classify_main(x, ref_centers))
    )

    ## (3) Single k, no reference image ##
  } else if (length(kcols) == 1 && is.null(refID) && interactive == FALSE) {
    message("Image classification in progress...")
    ifelse(imgsize < 100,
      outdata <- pbmclapply(imgdat, function(x) classify_main(x, kcols), mc.cores = cores),
      outdata <- lapply(imgdat, function(x) classify_main(x, kcols))
    )

    ## (4) Single k, interactively specified centre, with reference image ##
  } else if (!is.null(refID) && interactive == TRUE) {

    # Reference image
    refimg <- imgdat[[refID]]

    if (plotnew) dev.new(noRStudioGD = TRUE)

    plot(refimg, ...)

    if (!is.null(kcols)) {
      message(paste("Select the", kcols, "focal colours"))
      reference <- as.data.frame(locator(type = "p", col = col, n = kcols))
    } else if (is.null(kcols)) {
      message(paste0(
        "Select the focal colours in image ",
        attr(refimg, "imgname"), ", and press [esc] to continue."
      ))
      reference <- as.data.frame(locator(type = "p", col = col))
      kcols <- nrow(reference)
    }
    tag_loc <- reference

    if (plotnew) dev.off()

    ref_centers <- do.call(rbind, lapply(
      seq_len(nrow(reference)),
      function(x) as.data.frame(t(refimg[reference$x[x], reference$y[x], 1:3]))
    ))
    names(ref_centers) <- c("R", "G", "B")

    message("Image classification in progress...")
    ifelse(imgsize < 100,
      outdata <- pbmclapply(imgdat, function(x) classify_main(x, ref_centers), mc.cores = cores),
      outdata <- lapply(imgdat, function(x) classify_main(x, ref_centers))
    )

    ## (5) Multiple k, with interactively-specified centres for each image. ##
  } else if (is.null(refID) && interactive) {
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
    tag_loc <- list()
    i <- 1
    while (i <= length(imgdat)) {
      if (plotnew) dev.new(noRStudioGD = TRUE)

      plot(imgdat[[i]], ...)

      if (!is.null(n_cols_test)) {
        message(paste0(
          "Select the ", kcols[[i]], " focal colours in image ",
          attr(imgdat[[i]], "imgname", ".")
        ))
        reference <- as.data.frame(locator(type = "p", col = col, n = kcols[[i]]))
      } else if (is.null(n_cols_test)) {
        message(paste0(
          "Select the focal colours in image ",
          attr(imgdat[[i]], "imgname"), ", and press [esc] to continue."
        ))
        reference <- as.data.frame(locator(type = "p", col = col))
        kcols[[i]] <- nrow(reference)
      }
      if (plotnew) dev.off()

      ref_centers <- try(do.call(rbind, lapply(
        seq_len(nrow(reference)),
        function(x) as.data.frame(t(imgdat[[i]][reference$x[x], reference$y[x], 1:3]))
      )),
      silent = TRUE
      )
      centers[[i]] <- ref_centers
      tag_loc[[i]] <- reference

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
    message("Image classification in progress...")
    ifelse(imgsize < 100,
      outdata <- pbmclapply(seq_along(imgdat),
        function(x) classify_main(imgdat[[x]], centers[[x]]),
        mc.cores = cores
      ),
      outdata <- lapply(
        seq_along(imgdat),
        function(x) classify_main(imgdat[[x]], centers[[x]])
      )
    )
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
    # Reduce to single integer if multiple k's are all the same
    if (length(unique(kcols_i)) == 1) {
      kcols_i <- kcols_i[1]
    }
  }
  kcols_i
}
