#' Fast importing of spectral data files
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
#' @param subdir should subdirectories within the \code{where} folder be
#' included in the search? (defaults to \code{FALSE}).
#' @param subdir.names should subdirectory path be included in the name of the
#' spectra? (defaults to \code{FALSE}).
#' @return A data frame, of class \code{rspec}, containing individual imported
#' spectral files as columns.
#' Reflectance values are interpolated to the nearest wavelength integer.
#' 
#' @keywords internal
#' 
#' @examples \dontrun{
#' getspec('examplespec/', lim = c(400, 900))  
#' getspec('examplespec/', ext = 'ttt')}
#' 
#' @author Chad Eliason \email{cme16@@zips.uakron.edu}, Rafael Maia \email{rm72@@zips.uakron.edu}
#' 
#' @references Montgomerie R (2006) Analyzing colors. In: Hill G, McGraw K (eds) 
#' Bird coloration. Harvard University Press, Cambridge, pp 90-147.


#clumsy: if subdir=T, column name includes subdir name (desired?)

# CME: can we have this automatically set the lower and upper limits based on the spec 
#      files? These files are usually a pain to view in a text editor to see what wl 
#      range was used.
# RM: hm.. not sure I like this idea very much. two problems: (1) if specs have different 
#     ranges they can't be added to the same matrix without adding NAs; (2) If saved spec
#     includes a range that the spec can't actually read, it will be very noisy and may 
#     mess corrections (depending on how these are handled).
#   suggested solution: a secondary function that examines files and returns recorded WL 
#     range (in a dataframe or table)

getspecf2 <- function(where = getwd(), ext = 'txt', lim = c(300, 700), decimal = ".", 
                     subdir = FALSE, subdir.names = FALSE){

corrupt <- FALSE

extension <- paste('.', ext, sep='')

file_names <- list.files(where, pattern=extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',file_names,sep='')

cat(length(files),' files found; importing spectra\n')

if (subdir.names) {
  file_names <- gsub(extension,'',file_names)}else{
  file_names <- gsub(extension,'',basename(file_names))
}

if (length(file_names)==0) {
  stop('No files found. Try a different ext')
} 

range <- lim[1]:lim[2]

###
# START OF WHAT WAS A LOOP
###

raw <- sapply(files, scan, what='', quiet = T, 
  dec = decimal, sep = '\n', skipNul = TRUE, simplify=FALSE)

# rough fix for 'JazIrrad' files that have a stram of calibration data at the end
calib <- sapply(raw, grep, pattern='Begin Calibration Data', simplify=FALSE)
calibL <- !!unlist(lapply(calib, length))

if(any(calibL)){
for(i in seq_along(calibL))
  if(calibL[i])
    raw[[i]] <- raw[[i]][1:calib[[i]] - 1]
  }



# find last line with text
start <- lapply(raw, grep, pattern='[A-Da-dF-Zf-z]')

# correct for spectrasuite files, which have a "End Processed Spectral Data" at the end
isendline <- lapply(raw, grep, pattern='End.*Spectral Data')
isendline <- !!unlist(lapply(isendline, length))

if(any(isendline))
 for(i in seq_along(isendline))
   if(isendline[i]) start[[i]] <- start[[i]][-length(start[[i]])]

start <- unlist(lapply(start, max))

end <- unlist(lapply(raw, length)) - start

end[isendline] <- end[isendline] - 1

# Avantes has an extra skipped line between header and data. Bad Avantes.

newavaheader <- lapply(raw, grep, pattern="Wave.*;Sample.*;Dark.*;Reference;Reflectance")
newavaheader <- !!unlist(lapply(newavaheader, length))

start[newavaheader] <- start[newavaheader] + 1


# find if columns are separated by semicolon or tab

issem <- !!unlist(lapply(lapply(raw, grep, pattern=';'), length) )
istab <- !!unlist(lapply(lapply(raw, grep, pattern='\t'), length) )

if (any(apply(cbind(issem, istab), 1, all)))
  stop('inconsistent column delimitation in source files.')

separ <- ifelse(issem, ';', '\t')

# gitit
rawtab <- lapply(seq_along(raw), function(x)
  suppressWarnings(
  read.table(text=raw[[x]], dec = decimal, sep = separ[x], skip = start[x], nrows = end[x], 
    row.names = NULL, skipNul = TRUE, header=FALSE)
  )
  )

lastcol <- unlist(lapply(rawtab, ncol))

rawtab <- lapply(seq_along(rawtab), function(x) rawtab[[x]][c(1,lastcol[x])] )

if(any(unlist(lapply(rawtab, apply, 2, class)) != 'numeric'))
  corrupt <- TRUE

interp <- lapply(lapply(rawtab, approx, xout=range), '[', 'y')

interp <- do.call(cbind.data.frame, c(wl=list(range), interp))

names(interp) <- c("wl", as.character(strsplit(file_names, extension)))

class(interp) <- c('rspec','data.frame')

if(corrupt){
  cat('\n')
  warning('one or more files contains character elements within wavelength and/or reflectance values - check for corrupt or otherwise poorly exported files. Verify values returned.')
  	}


interp

}

