#' Import image data
#'
#' Finds and imports PNG, JPEG, and/or BMP images.
#'
#' @param imgpath (required) either the full path to a given image (including extension),
#' or the path to a folder in which multiple image files are located. Mixed file formats
#' within a folder are accepted.
#' @param subdir should subdirectories within the \code{imgpath} folder be
#' included in the search? (defaults to \code{FALSE}).
#' @param subdir.names should subdirectory path be included in the name of the
#' images? (defaults to \code{FALSE}).
#' @param max.size maximum size of all images to be allowed in memory, in GB. Defaults to
#' \code{1}.
#' @param cores number of cores to be used in parallel processing. If \code{1}, or
#' if total image sizes exceed 200 mb in memory, parallel computing will not be used.
#' Defaults to \code{getOption("mc.cores", 2L)}. Not available on Windows.
#'
#' @return a image, or list of images, of class \code{rimg},
#' for use in further \code{pavo} functions.
#'
#' @export
#'
#' @importFrom pbmcapply pbmclapply
#' @importFrom imager load.image
#'
#' @examples \dontrun{
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = 'pavo'))
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = 'pavo'))
#' }
#'
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

getimg <- function(imgpath = getwd(), subdir = FALSE, subdir.names = FALSE, max.size = 1, cores = getOption("mc.cores", 2L)) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Allowed extensions
  ext <- c("jpg", "jpeg", "png", "bmp")

  ## Cores
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
  }

  ## ------------------------------ Main ------------------------------ ##

  # If file extensions are in 'imgpath', it's a single image being directly specified
  if (grepl(paste(ext, collapse = "|"), imgpath, ignore.case = TRUE)) {
    imgdat <- load.image(imgpath)

    imgdat <- as.rimg(drop(as.array(imgdat)), name = sub(".*\\/", "", sub("[.][^.]+$", "", imgpath)))

    # Warn of slowness if dimensions are large
    if ((dim(imgdat)[1] * dim(imgdat)[2]) > (1000 * 1000)) {
      message("Image dimensions are relatively large, consider reducing image size with procimg() for faster performance.")
    }

    # Otherwise it's a directory of images
  } else if (!grepl(paste(ext, collapse = "|"), imgpath)) {

    # Set allowed file extensions
    extension <- paste0("\\.", ext, "$", collapse = "|")

    # File names
    file_names <- list.files(imgpath, pattern = extension, recursive = subdir, include.dirs = subdir)
    files <- paste(imgpath, "/", file_names, sep = "")

    if (subdir.names) {
      file_names <- gsub(extension, "", file_names)
    } else {
      file_names <- gsub(extension, "", basename(file_names))
    }

    if (length(file_names) == 0) {
      stop("No files found at specified location.")
    }

    message(length(files), " files found; importing images.")

    if (length(file_names) == 0) {
      stop("No .jpg, .jpeg, .png, or .bmp files found in specified location.")
    }

    imgnames <- gsub(extension, "", file_names)

    # Stop if max size estimated to exceed available memory
    imgsize <- prod(dim(load.image(files[1])))
    totalsize <- ((imgsize * 8) * length(file_names)) / (1024^3)
    if (totalsize > max.size) {
      stop("Total size of images likely exceeds available memory. Check max.size is set appropriately.")
    }

    # Warn of slowness if size is large
    if (totalsize > 0.2) {
      message("Total size of images exceeds 200 mb in memory, which may result in slowed performance. Consider resizing images with procimg() prior to analysis, if speed is a priority.")
    }

    # Crudely avoid a bug in pbmclapply when handling large objects.
    if (totalsize < 0.1) {
      imgdat <- pbmclapply(1:length(file_names), function(x) load.image(files[x]), mc.cores = cores)
      imgdat <- lapply(1:length(imgdat), function(x) drop(as.array(imgdat[[x]])))
    } else {
      imgdat <- lapply(1:length(file_names), function(x) load.image(files[x]))
      imgdat <- lapply(1:length(imgdat), function(x) drop(as.array(imgdat[[x]])))
    }

    imgdat <- as.rimg(imgdat, imgnames)

    # Simplify if it's a single image  (TODO LESS SHITE)
    if (length(imgdat) == 1) imgdat <- cimg2rimg(imgdat[[1]])
  }
  imgdat
}
