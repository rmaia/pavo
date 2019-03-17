#' Import image data
#'
#' Finds and imports PNG, JPEG, and/or BMP images.
#'
#' @param imgpath (required) eith the full file-path or URL to an image (including extension),
#' or the path to a folder in which multiple image files are located. Mixed file formats
#' within a folder are accepted.
#' @param subdir should subdirectories within the \code{imgpath} folder be
#' included in the search? (defaults to \code{FALSE}).
#' @param subdir.names should subdirectory path be included in the name of the
#' images? (defaults to \code{FALSE}).
#' @param max.size maximum size of all images to be allowed in memory, in GB. Defaults to
#' \code{1}.
#' @param cores deprecated argument.
#'
#' @return a image, or list of images, of class \code{rimg},
#' for use in further \code{pavo} functions.
#'
#' @importFrom magick image_info
#'
#' @export
#'
#' @examples
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
#' 
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' 
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

getimg <- function(imgpath = getwd(), subdir = FALSE, subdir.names = FALSE,
                   max.size = 1, cores) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Allowed extensions
  ext <- c("jpg", "jpeg", "png", "bmp")

  ## Cores
  if (!missing(cores)) {
    warning("the cores argument is deprecated as all image importing is now vectorised.", 
            call. = FALSE)
  }

  ## ------------------------------ Main ------------------------------ ##

  # If file extensions are in 'imgpath', it's a single image being directly specified
  if (grepl(paste(ext, collapse = "|"), imgpath, ignore.case = TRUE)) {
    
    imgdat <- magick2rimg(image_read(imgpath), name = sub(".*\\/", "", sub("[.][^.]+$", "", imgpath)))

    # Warn of slowness if dimensions are large
    if (dim(imgdat)[1] * dim(imgdat)[2] > 1000000) {
      message("Image dimensions are relatively large, consider reducing image size with procimg() for faster performance.")
    }

    # Otherwise it's a directory of images
  } else {

    # Set allowed file extensions
    extension <- paste0("\\.", ext, "$", collapse = "|")

    # File names
    file_names <- list.files(imgpath,
      pattern = extension,
      recursive = subdir, include.dirs = subdir
    )
    files <- paste0(imgpath, "/", file_names)

    if (subdir.names) {
      file_names <- gsub(extension, "", file_names)
    } else {
      file_names <- gsub(extension, "", basename(file_names))
    }

    if (length(file_names) == 0) {
      stop("No files found at specified location.")
    }

    message(length(files), " files found; importing images.")

    imgnames <- gsub(extension, "", file_names)

    # Stop if max size estimated to exceed available memory
    imgsize <- image_info(image_read(files[1]))['filesize']
    totalsize <- ((imgsize * 8) * length(file_names)) / (1024^3)
    if (totalsize > max.size) {
      stop("Total size of images likely exceeds available memory. Check max.size is set appropriately.")
    }

    # Warn of slowness if size is large
    if (totalsize > 0.2) {
      message("Total size of images exceeds 200 mb in memory, which may result in slowed performance. Consider resizing images with procimg() prior to analysis, if speed is a priority.")
    }

    # Get images
      imgdat <- magick2rimg(image_read(files), name = imgnames)
    
    # Simplify if it's a single image   ###TODO###
    if (length(imgdat) == 1) imgdat <- cimg2rimg(imgdat[[1]])
  }
  imgdat
}

## Rotate matrices 90-degrees
rot90 <- function(x) {
  permVec <- c(2, 1, 3:length(dim(x)))
  rotA <- aperm(x, permVec)
  rotA <- rotA[dim(x)[2]:1, , ]
  rotA
}

## Mirror matrices about x axis
mirrorx <- function(x) {
  if (length(dim(x)) == 3) {
    for (i in seq_len(dim(x)[3])) {
      x[, , i] <- x[, , i][, rev(seq_len(ncol(x[, , i])))]
    }
  } else {
    x <- x[, rev(seq_len(ncol(x)))]
  }
  x
}
