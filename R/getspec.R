#' Import spectra files
#'
#' Finds and imports spectra files from a folder. Currently works
#' for reflectance files generated in Ocean Optics SpectraSuite (USB2000,
#' USB4000 and Jaz spectrometers), CRAIC software (after exporting) and
#' Avantes (before or after exporting).
#'
#' @param where (required) folder in which files are located.
#' @param ext file extension to be searched for, without the "."
#' (defaults to "txt").
#' @param lim a vector with two numbers determining the wavelength limits to be
#' considered (defaults to 300 and 700).
#' @param decimal character to be used to identify decimal plates
#' (defaults to ".").
#' @param sep column delimiting characters to be considered in addition to the
#' default (which are: tab, space, and ";")
#' @param subdir should subdirectories within the \code{where} folder be
#' included in the search? (defaults to \code{FALSE}).
#' @param subdir.names should subdirectory path be included in the name of the
#' spectra? (defaults to \code{FALSE}).
#' @param cores Number of cores to be used. If greater than 1, import will use
#'  parallel processing (not available in Windows).
#' @param ignore.case Logical. Should the extension search be case insensitive?
#' (defaults to TRUE)
#' @return A data frame, of class \code{rspec}, containing individual imported
#' spectral files as columns.
#' Reflectance values are interpolated to the nearest wavelength integer.
#'
#' @export
#'
#' @importFrom pbmcapply pbmclapply
#'
#' @examples
#' getspec(system.file("testdata", package = "pavo"), lim = c(400, 900))
#' getspec(system.file("testdata", package = "pavo"), ext = "ttt")
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @author Hugo Gruson \email{hugo.gruson+R@@normalesup.org}
#'
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds)
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


# clumsy: if subdir=T, column name includes subdir name (desired?)

getspec <- function(where = getwd(), ext = "txt", lim = c(300, 700), decimal = ".",
                    sep = NULL, subdir = FALSE, subdir.names = FALSE,
                    cores = getOption("mc.cores", 2L), ignore.case = TRUE) {

  # allow multiple extensions
  extension <- paste0("\\.", ext, "$", collapse = "|")

  # get file names
  file_names <- list.files(where,
    pattern = extension, ignore.case = ignore.case,
    recursive = subdir, include.dirs = subdir
  )
  print(summary(file_names))

  file_names <- sort(file_names, method = "radix")

  nb_files <- length(file_names)

  if (nb_files == 0) {
    stop('No files found. Try a different extension value for argument "ext"')
  }

  files <- paste0(where, "/", file_names)

  if (subdir.names) {
    specnames <- gsub(extension, "", file_names, ignore.case = ignore.case)
  } else {
    specnames <- gsub(extension, "", basename(file_names), ignore.case = ignore.case)
  }

  # Wavelength range
  range <- seq(lim[1], lim[2])

  # define separators
  seps <- paste0(c("\\\t|\\;| ", sep), collapse = "|\\")


  # Setting a progress bar
  # progbar <- txtProgressBar(min = 0, max = length(files), style = 2)

  # On Windows, set cores to be 1
  if (cores > 1 && .Platform$OS.type == "windows") {
    cores <- 1
    message('Parallel processing not available in Windows; "cores" set to 1.\n')
  }

  if (nb_files <= cores) {
    cores <- 1
  }

  # if ProcSpec, check if xml2 is installed and loaded
  if (any(grepl("\\.ProcSpec$", files, ignore.case = ignore.case))) {
    if (!requireNamespace("xml2", quietly = TRUE)) {
      stop('"xml2" package needed for to import .ProcSpec files. Please install it.',
        call. = FALSE
      )
    }

    if (!isNamespaceLoaded("xml2")) {
      requireNamespace("xml2")
    }
  }

  # message with number of spectra files being imported
  message(nb_files, " files found; importing spectra:")

  gsp <- function(ff) {
    if (grepl("\\.ProcSpec$", ff, ignore.case = ignore.case)) {
      # ProcSpec files differ too much from the other formats and need their own
      # function.

      tempframe <- parse_procspec(ff)
    } else if (grepl("\\.(ABS|TRM)$", ff, ignore.case = ignore.case)) {
      tempframe <- parse_avantes(ff)
    } else {

      # read in raw file
      raw <- scan(
        file = ff, what = "", quiet = TRUE,
        dec = decimal, sep = "\n", skipNul = TRUE
      )

      # rough fix for 'JazIrrad' files that have a stram of calibration data at the end
      if (any(grepl("Begin Calibration Data", raw))) {
        raw <- raw[1:grep("Begin Calibration Data", raw) - 1]
      }

      # ToDo we can actually use this raw string to import metadata if we want

      # substitute separators for a single value to be used in split
      raw <- gsub(seps, ";", raw)

      # remove multiply occuring split character
      raw <- gsub(";+", ";", raw)

      # remove split character from first or last occurence
      raw <- gsub("^;|;$", "", raw)

      # convert decimal value to point
      raw <- gsub(decimal, ".", raw, fixed = TRUE)

      # exclude lines that have text
      # raw <- raw[!grepl('[A-Da-dF-Zf-z]', raw)]

      # exclude any line that doesn't start with a number
      scinum <- "-?[[:digit:]]+\\.?[[:digit:]]*((E|e)(-|\\+)?[[:digit:]]+)?"
      raw <- raw[grepl(paste0("^", scinum, ";"), raw)]

      # split on separators
      rawsplit <- strsplit(raw, ";")

      rawsplit <- do.call(rbind, rawsplit)

      if (dim(rawsplit)[2] < 2) {
        stop('could not separate columns, choose a different value for "sep" argument', call. = FALSE)
      }

      # convert to numeric, check for NA
      suppressWarnings(class(rawsplit) <- "numeric")

      # remove columns where all values are NAs (due to poor tabulation)
      rawsplit <- rawsplit[, !apply(rawsplit, 2, function(x) all(is.na(x)))]

      # use only first and last column
      tempframe <- rawsplit[, c(1, dim(rawsplit)[2])]
    }

    # return interpolated spec
    interp <- approx(tempframe[, 1], tempframe[, 2], xout = range)$y

    # setTxtProgressBar(progbar, i)
  }


  tmp <- pbmclapply(files, function(x)
    tryCatch(gsp(x),
      error = function(e) NULL,
      warning = function(e) NULL
    ), mc.cores = cores)

  if (any(unlist(lapply(tmp, is.null)))) {
    whichfailed <- which(unlist(lapply(tmp, is.null)))
    # stop if all files are corrupt
    if (length(whichfailed) == nb_files) {
      stop("Could not import spectra, check input files and function arguments", call. = FALSE)
    }

    # if not, import the ones remaining
    warning("Could not import one or more files:\n",
      paste0(files[whichfailed], "\n"),
      call. = FALSE
    )

    specnames <- specnames[-whichfailed]
  }


  tmp <- do.call(cbind, tmp)

  final <- cbind(range, tmp)

  colnames(final) <- c("wl", specnames)
  final <- as.data.frame(final)
  class(final) <- c("rspec", "data.frame")

  # Negative value check
  if (any(final < 0, na.rm = TRUE)) {
    message(
      "\nThe spectral data contain ", sum(final < 0, na.rm = TRUE),
      " negative value(s), which may produce unexpected results if used in models. Consider using procspec() to correct them."
    )
  }

  final
}
