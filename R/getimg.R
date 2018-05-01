#' Import image data
#'
#' Finds and imports PNG, JPEG, or BMP images.
#'
#' @param imgpath (required) either the full path to a given image (including extension),
#' or the path to a folder in which multiple image files are located. Mixed file formats
#' within a folder are fine.
#' @param subdir should subdirectories within the \code{where} folder be
#' included in the search? (defaults to \code{FALSE}).
#' @param max.size maximum size of all images allowed in memory, in GB. Defaults to
#' \code{2}.
#' @param cores number of cores to be used in parallel processing. If \code{1}, parallel
#'  computing will not be used. Defaults to \code{getOption("mc.cores", 2L)}.
#'
#' @return a array, or list of arrays, of class \code{rimg}, containing image data
#' for use in further \code{pavo} functions.
#'
#' @export
#' 
#' @importFrom pbmcapply pbmclapply
#' @importFrom readbitmap read.bitmap
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

getimg <- function(imgpath = getwd(), subdir = FALSE, max.size = 2, cores = getOption("mc.cores", 2L)) {

  ## Checks
  # Allowed extensions
  ext <- c("jpg", "jpeg", "png", "bmp")
  # Cores
  if (cores > 1 && .Platform$OS.type == "windows") {  
    cores <- 1
    message('Parallel processing not available in Windows; "cores" set to 1\n')
  }

  # If file extensions are in 'imgpath', it's a single image being directly specified
  if (isTRUE(grepl(paste(ext, collapse = "|"), imgpath, ignore.case = TRUE))) {
    imgdat <- read.bitmap(imgpath)

    # Duplicate channels if grayscale
    if (is.na(dim(imgdat)[3])) {
      imgdat <- replicate(3, imgdat, simplify = "array")
    }

    # Attributes
    attr(imgdat, "imgname") <- sub(".*\\/", "", sub("[.][^.]+$", "", imgpath))
    attr(imgdat, "scale") <- NULL
    attr(imgdat, "state") <- "raw"
    class(imgdat) <- c("rimg", "array")

    # Otherwise it's a directory of images
  } else if (!isTRUE(grepl(paste(ext, collapse = "|"), imgpath))) {

    # Set allowed file extensions
    extensions <- paste0("\\.", ext, "$", collapse = "|")

    # File names
    file_names <- list.files(imgpath, pattern = extensions, recursive = subdir, include.dirs = subdir)
    files <- paste(imgpath, "/", file_names, sep = "")

    message(length(files), " files found; importing images.")

    if (length(file_names) == 0) {
      stop("No .jpg, .jpeg, .png, or .bmp files found in specified location.")
    }

    imgnames <- gsub(extensions, "", file_names)
    
    # Stop if max size estimated to exceed available memory
    imgsize <- prod(dim(read.bitmap(files[1])))
    totalsize <- ((imgsize * 8) * length(file_names)) / (1024^3)
    if(totalsize > max.size)
      stop("Total size of images likely exceeds available memory")

    imgdat <- pbmclapply(1:length(file_names), function(x) read.bitmap(files[x]), mc.cores = cores)
    
    # Duplicate channels if grayscale
    for(i in 1:length(imgdat)){  
      if(is.na(dim(imgdat[[i]])[3]))
        imgdat[[i]] <- replicate(3, imgdat[[i]], simplify = "array")
    }

    # Attributes
    for (i in 1:length(file_names)) {
      attr(imgdat[[i]], "imgname") <- imgnames[i]
      attr(imgdat[[i]], "scale") <- NULL
      attr(imgdat[[i]], "state") <- "raw"
      class(imgdat[[i]]) <- c("rimg", "array")
    }
    # the list itself needs attributes 
     class(imgdat) <- c("rimg", "list")
     attr(imgdat, 'state') <- 'raw'
  }
  imgdat
}
