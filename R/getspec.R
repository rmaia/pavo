#' Import spectra files
#' 
#' Finds and imports spectra files from a folder. Currently works
#' for reflectance files generated in Ocean Optics SpectraSuite (USB2000,
#' USB4000 and Jaz spectrometers), CRAIC software (after exporting) and 
#' Avantes (after exporting).
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
#' @param fast logical. if \code{TRUE}, will try a fast algorithm that assumes
#'  all spectra were produced using the same software configuration (defaults
#'  to \code{FALSE}).
#' @return A data frame, of class \code{rspec}, containing individual imported
#' spectral files as columns.
#' Reflectance values are interpolated to the nearest wavelength integer.
#' 
#' @export
#' 
#' @examples \dontrun{
#' getspec('examplespec/', lim = c(400, 900))	
#' getspec('examplespec/', ext = 'ttt')}
#' 
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' 
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


#clumsy: if subdir=T, column name includes subdir name (desired?)


getspec <- function(where = getwd(), ext = 'txt', lim = c(300, 700), decimal = ".", 
           sep=NULL, subdir = FALSE, subdir.names = FALSE, fast = FALSE)
  {
  
  if(fast){
    cat("Attempting fast import. ")
    fastcatch <- try(getspecf(where = where, ext = ext, lim = lim, decimal = decimal, 
      subdir = subdir, subdir.names = subdir.names), silent=TRUE)
    
    if('try-error' %in% class(fastcatch)){
    	message('Fast import failed.\nSwitching to comprehensive evaluation.\n')
    }else{
        message('Fast import successful.\n NOTE: Fast import only works if all spectra have the same output format. Check!')
        return(fastcatch)
    }
    
    
  }
  
  # allow multiple extensions
  extension <- paste0("\\.",ext, collapse='|')
 
  # get file names
  file_names <- list.files(where, pattern = extension, recursive = subdir, include.dirs = subdir)
  files <- paste(where, '/', file_names, sep = '')
  
  cat(length(files), ' files found; importing spectra\n')
  
  if(subdir.names){
  	file_names <- gsub(extension, '', file_names)}else{
  	file_names <- gsub(extension, '', basename(file_names))
  	 }
  
  if(length(file_names) == 0){
  	stop('No files found. Try a different extension value for argument "ext"')
  	} 
  
  # Wavelength range
  range <- seq(lim[1],lim[2])
  
  # Build shell of final output
  final <- matrix(nrow = length(range), ncol = length(file_names) + 1)
  final[, 1] <- range
  
  # vector of corrupt files
  corrupt <- rep(FALSE, length(files))
  
  # define separators 
  seps <- paste0(c("\\\t|\\;| ",sep), collapse="|\\")

  
  # Setting a progress bar
  progbar <- txtProgressBar(min = 0, max = length(files), style = 2)
  
  for(i in 1:length(files))
    {
  
    # read in raw file
    raw <- scan(file = files[i], what = '', quiet = T, dec = decimal, sep = '\n', skipNul = TRUE)
    
    # rough fix for 'JazIrrad' files that have a stram of calibration data at the end
    if(length(grep('Begin Calibration Data', raw)) > 0)
       raw <- raw[1:grep('Begin Calibration Data', raw) - 1]
    
    #ToDo we can actually use this raw string to import metadata if we want
    
      # TEMPORARY PLACEHOLDER TO DEAL WITH NaN & Inf VALUES IN SPEC
      # remove NaN & inf
      #    if(length(grep('\\tnan', raw)) + length(grep('\\tinf', raw)) > 0){    
      #      raw <- gsub('\\tnan', '\t0.0', raw)
      #      raw <- gsub('\\tinf', '\t0.0', raw)
      #      corrupt <- TRUE
      #    }
    
    # exclude columns that have text
    raw <- raw[!grepl(paste0('[A-Da-dF-Zf-z]'), raw)]
        
    # substitute separators for a single value to be used in split
    raw <- gsub(seps, ";", raw)
    
    # remove multiply occuring split character
    raw <- gsub(paste0("(;)\\1+"), "\\1", raw)
    
    # remove split character from first or last occurence
    raw <- gsub('^;|;$', '', raw)

    # split on separators
    rawsplit <- strsplit(raw, seps)
    
    rawsplit <- do.call(rbind, rawsplit)
    
    if(dim(rawsplit)[2] < 2)
      stop('could not separate columns, choose a different value for "sep" argument', call.=FALSE)
   
   # convert to numeric, check for NA
   suppressWarnings(class(rawsplit) <- 'numeric')
   
   # remove columns where all values are NAs (due to poor tabulation)
   rawsplit <- rawsplit[, !apply(rawsplit, 2, function(x) all(is.na(x)))]
    
   # use only first and last column
   tempframe <- rawsplit[, c(1, dim(rawsplit)[2])]
   
   # interpolate
   interp <- do.call(cbind, approx(tempframe[, 1], tempframe[, 2], xout = range))
   
   # check if there are any NA left, assign as corrupt if so
   if(any(is.na(interp)))
     corrupt[i] <- TRUE

   # add to final table
   final[, i+1] <- interp[, 2]
   
   head(final)
    
    setTxtProgressBar(progbar, i)
    
    }
  
  colnames(final) <- c('wl', gsub(extension, '', file_names))
  final <- as.data.frame(final)
  class(final) <- c('rspec', 'data.frame')
  
if(any(corrupt)){
  	cat('\n')
  	warning('the following files contain character elements within wavelength and/or reflectance values: - check for corrupt or otherwise poorly exported files. Verify values returned.')
}
  
  # Negative value check
  if(length(final[final < 0]) > 0){
    message(paste("\nThe spectral data contain ", length(final[final < 0]), " negative value(s), which may produce unexpected results if used in models. Consider using procspec() to correct them."))
  }

  final
  }
