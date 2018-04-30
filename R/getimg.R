#' Import image data
#'
#' Finds and imports PNG, JPEG, or BMP images.
#'
#' @param imgpath (required) either the full path to a given image (including extension),
#' or the path to a folder in which multiple image files are located. Mixed file formats
#' within a folder are fine.
#'
#' @return a array, or list of arrays, of class \code{rimg}, containing image data 
#' for use in further \code{pavo} functions.
#'
#' @export
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

getimg <- function(imgpath = getwd()) {
  ext <- c("jpg", "jpeg", "png", "bmp") # Allowed extensions

  # If file extensions are in imgpath, it's a single image being directly specified
  if (isTRUE(grepl(paste(ext, collapse = "|"), imgpath, ignore.case = TRUE))) {
    imgdat <- readbitmap::read.bitmap(imgpath)

    # Duplicate channels if grayscale
    if(is.na(dim(imgdat)[3])){
      imgdat <- replicate(3, imgdat, simplify = "array")
    }

    # Attributes
    attr(imgdat, "imgname") <- sub(".*\\/", "", sub("[.][^.]+$", "", imgpath))
    attr(imgdat, "scale") <- NULL
    attr(imgdat, 'state') <- 'raw'
    class(imgdat) <- c("rimg", "array")

    # Otherwise it's a directory of images
  } else if (!isTRUE(grepl(paste(ext, collapse = "|"), imgpath))) {

    # Set allowed file extensions
    extensions <- paste0("\\.", ext, "$", collapse = "|")

    # File names
    file_names <- list.files(imgpath, pattern = extensions, recursive = FALSE, include.dirs = FALSE)
    files <- paste(imgpath, "/", file_names, sep = "")

    message(length(files), ' files found; importing images.')

    if (length(file_names) == 0) {
      stop("No .jpg, .jpeg, .png, or .bmp files found in specified location.")
    }

    imgnames <- gsub(extensions, "", file_names)

    imgdat <- lapply(1:length(file_names), function(x) readbitmap::read.bitmap(files[x]))
    # for(i in 1:length(imgdat)){  # b & w images?
    #   if(is.na(dim(imgdat[[i]])[3]))
    #     imgdat[[i]] <- replicate(3, imgdat[[i]], simplify = "array")
    # }

    # Attributes
    for (i in 1:length(file_names)) {
      attr(imgdat[[i]], "imgname") <- imgnames[i]
      attr(imgdat[[i]], "scale") <- NULL
      attr(imgdat[[i]], 'state') <- 'raw'
      class(imgdat[[i]]) <- c("rimg", "array")
    }
    #class(imgdat) <- c("ptrn", "list")
  }
  imgdat
}
