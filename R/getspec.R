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
#' (defaults to ".")
#' @param subdir should subdirectories within the \code{where} folder be
#' included in the search? (defaults to \code{FALSE})
#' @param subdir.names should subdirectory path be included in the name of the
#' spectra? (defaults to \code{FALSE})
#' @return A data frame, of class \code{rspec}, containing individual imported
#' spectral files as columns.
#' Reflectance values are interpolated to the nearest wavelength integer.
#' @export
#' @examples \dontrun{
#' getspec('examplespec/', lim=c(400,900))	
#' getspec('examplespec/', ext='ttt')}
#' @author Rafael Maia \email{rm72@@zips.uakron.edu}
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


#clumsy: if subdir=T, column name includes subdir name (desired?)


getspec <- function(where=getwd(), ext='txt', lim=c(300,700), decimal=".", 
           subdir=FALSE, subdir.names=FALSE)
  {
  
  corrupt <- FALSE
  
  extension <- paste('.', ext, sep='')
  
  file_names <- list.files(where, pattern = extension, recursive = subdir, include.dirs = subdir)
  files <- paste(where, '/', file_names,sep='')
  
  cat(length(files), ' files found; importing spectra\n')
  
  if(subdir.names){
  	file_names <- gsub(extension, '', file_names)}else{
  	file_names <- gsub(extension, '', basename(file_names))
  	 }
  
  if(length(file_names) == 0){
  	stop('No files found. Try a different ext')
  	} 
  
  range <- lim[1]:lim[2]
  
  final <- data.frame(matrix(nrow = length(range), ncol = length(file_names) + 1))
  final[, 1] <- range
  
  # Setting a progress bar
  progbar <- txtProgressBar(min = 0, max = length(files), style = 2)
  
  for(i in 1:length(files))
    {
  
    raw <- scan(file = files[i], what = '', quiet = T, dec = decimal, sep = '\n', skipNul = TRUE)
    #ToDo we can actually use this raw string to import metadata if we want
    
    # find last line with text
    # correct for spectrasuite files, which have a "End Processed Spectral Data" at the end
    
    start <- grep('[A-Da-dF-Zf-z]', raw)
    isendline <- length(grep('End.*Spectral Data', raw)) > 0
    
    if(isendline)
      start <- start[-length(start)]
    
    start <- max(start)
    end <- length(raw) - start
    
    if(isendline > 0)
      end <- end - 1
    
    # Avantes has an extra skipped line between header and data. Bad Avantes.
    newavaheader <- length(grep("Wave.*;Sample.*;Dark.*;Reference;Reflectance", raw)) > 0
    
    if(newavaheader)
      start <- start + 1
    
    # find if columns are separated by semicolon or tab
    issem <- length(grep(';', raw)) > 0
    istab <- length(grep('\t', raw)) > 0
    
    if(issem & istab)
      stop('inconsistent column delimitation in source files.')
      
    separ <- ifelse(issem, ';', '\t')
    
    # extract data from file
    
    tempframe <- suppressWarnings(read.table(files[i], dec = decimal, sep = separ, skip = start, nrows = end, row.names = NULL, skipNul = TRUE))
    
    if(any(c('character','factor') %in% apply(tempframe, 2, class))){
      tempframe <- suppressWarnings(apply(tempframe, 2, 
        function(x) as.numeric(as.character(x))))
      
      if(sum(apply(tempframe, 2, function(x) sum(is.na(x))))) 
        corrupt <- TRUE
      }
      
    
    # remove columns where all values are NAs (due to poor tabulation)
    tempframe <- tempframe[ , colSums(is.na(tempframe)) < nrow(tempframe)]
    
    # Jaz and Avasoft8 have 5 columns, correct
    tempframe <- tempframe[ , c(1, dim(tempframe)[2])]
    
    
    interp<-data.frame(approx(tempframe[, 1], tempframe[, 2], xout = range))
    names(interp) <- c("wavelength", strsplit(file_names[i], extension))
    
    final[, i+1] <- interp[, 2]
    
    setTxtProgressBar(progbar, i)
    
    }
  
  names(final) <- c('wl', gsub(extension, '', file_names))
  class(final) <- c('rspec', 'data.frame')
  
  if(corrupt) warning('one or more files contains character elements within wavelength and/or reflectance values - check for corrupt or otherwise poorly exported files. Verify values returned.')
  	
  final
  }
