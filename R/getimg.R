#' Import image data
#'
#' Finds and imports PNG, JPEG, and/or BMP images.
#'
#' @param imgpath (required) either the full file-path or URL to an image (including extension),
#' or the path to a folder in which multiple image files are located. Mixed file formats
#' within a folder are accepted.
#' @param subdir should subdirectories within the `imgpath` folder be
#' included in the search? (defaults to `FALSE`).
#' @param subdir.names should subdirectory path be included in the name of the
#' images? (defaults to `FALSE`).
#' @param max.size maximum size of all images to be allowed in memory, in GB. Defaults to
#' `1`.
#' @param cores deprecated argument.
#'
#' @return a image, or list of images, of class `rimg`, for use in further
#' `pavo` functions.
#'
#' @importFrom magick image_info
#' @importFrom tools file_path_sans_ext
#'
#' @export
#'
#' @examples
#' # Single image
#' papilio <- getimg(system.file("testdata/images/papilio.png", package = "pavo"))
#'
#' # Multiple images
#' snakes <- getimg(system.file("testdata/images/snakes", package = "pavo"))
#' @author Thomas E. White \email{thomas.white026@@gmail.com}

getimg <- function(imgpath = getwd(), subdir = FALSE, subdir.names = FALSE,
                   max.size = 1, cores) {

  ## ------------------------------ Checks ------------------------------ ##

  ## Allowed extensions
  ext <- c("jpg", "jpeg", "png", "bmp")

  ## Cores
  if (!missing(cores)) {
    warning("the cores argument is deprecated as all image importing is now vectorised.",
      call. = FALSE
    )
  }

  ## ------------------------------ Main ------------------------------ ##

  # If file extensions are in 'imgpath', it's a single image being directly specified
  if (grepl(paste(ext, collapse = "|"), imgpath, ignore.case = TRUE)) {
    imgdat <- as.rimg(image_read(imgpath), name = file_path_sans_ext(basename(imgpath)))

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
    imgsize <- image_info(image_read(files[1]))["filesize"]
    totalsize <- ((imgsize * 8) * length(file_names)) / (1024^3)
    if (totalsize > max.size) {
      stop("Total size of images likely exceeds available memory. Check max.size is set appropriately.")
    }

    # Warn of slowness if size is large
    if (totalsize > 0.2) {
      message("Total size of images exceeds 200 mb in memory, which may result in slowed performance. Consider resizing images with procimg() prior to analysis, if speed is a priority.")
    }

    # Get images
    imgdat <- as.rimg(image_read(files), name = imgnames)

    # Simplify if it's a single image   ###TODO###
    if (length(imgdat) == 1) imgdat <- imgdat[[1]]
  }
  imgdat
}
