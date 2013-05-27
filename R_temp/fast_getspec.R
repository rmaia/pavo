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

# CME: can we have this automatically set the lower and upper limits based on the spec 
#      files? These files are usually a pain to view in a text editor to see what wl 
#      range was used.
# RM: hm.. not sure I like this idea very much. two problems: (1) if specs have different 
#     ranges they can't be added to the same matrix without adding NAs; (2) If saved spec
#     includes a range that the spec can't actually read, it will be very noisy and may 
#     mess corrections (depending on how these are handled).
#   suggested solution: a secondary function that examines files and returns recorded WL 
#     range (in a dataframe or table)

getspec3 <- function(where=getwd(), ext='txt', lim=c(300,700), decimal=".", 
           subdir=FALSE, subdir.names=FALSE)

{

extension <- paste('.', ext, sep='')

# ### remove
# where = "~/Desktop/glossystarlings"
# subdir=FALSE
# extension="ttt"
# decimal="."
###

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

# final <- data.frame(matrix(nrow=length(range), ncol=length(file_names)+1))
# final[,1] <- range

# Setting a progress bar
# progbar <- txtProgressBar(min=0, max=length(files), style=2)

raw <- scan(file = files[1], what = '', quiet = T, dec = decimal, sep = '\n')

#ToDo we can actually use this raw string to import metadata if we want

# find last line with text
# correct for spectrasuite files, which have a "End Processed Spectral Data" at the end

start <- grep('[A-Da-dF-Zf-z]',raw)
isendline <- length(grep('End.*Spectral Data', raw)) > 0

if (isendline) {
  start <- start[-length(start)]
}

start <- max(start)

end <- length(raw) - start

if (isendline > 0) {
  end <- end - 1
}

# Avantes has an extra skipped line between header and data. Bad Avantes.
newavaheader <- length(grep("Wave.*;Sample.*;Dark.*;Reference;Reflectance", raw)) > 0

if(newavaheader) {
  start <- start+1
}

# find if columns are separated by semicolon or tab
issem <- length(grep(';', raw)) > 0
istab <- length(grep('\t', raw)) > 0

if (issem & istab) {
  stop('inconsistent column delimitation in source files.')
}

separ <- ifelse(issem, ';', '\t')


rawtab <- read.table(files[1], skip=start, nrows=end, dec=decimal, sep=separ, header=FALSE)

wl <- rawtab[, 1]

# set what type of data are in columns (null causes not to read)
# Jaz and Avasoft8 have 5 columns, correct
numcols <- dim(rawtab)[2]
colClasses <- c(rep("NULL", numcols-1), "numeric")

# read data
read.all <- lapply(files, read.table, skip = start, nrows = end, dec = decimal, sep = separ, header = FALSE, colClasses = colClasses)

# combine columns
tempframe <- as.data.frame(do.call(cbind, read.all))

# remove columns where all values are NAs (due to poor tabulation)
# tempframe <- tempframe[, colSums(is.na(tempframe)) < nrow(tempframe)]

interp <- sapply(1:ncol(tempframe), FUN = function(x) {approx(x = wl, y = tempframe[, x], xout = range)$y})

interp <- as.data.frame(cbind(wl, interp))

names(interp) <- c("wl", as.character(strsplit(file_names, extension)))

class(interp) <- c('rspec','data.frame')

interp

}






# system.time(specs1 <- getspec('~/Desktop/glossystarlings', 'ttt'))
# system.time(specs2 <- getspec2('~/Desktop/glossystarlings', 'ttt'))
# system.time(specs3 <- getspec3('~/Desktop/glossystarlings', 'ttt'))

# # library(pavo)

# # setwd("/Users/chad/github/pavo/examplespec")

# getspec(ext='txt')
# getspec2(ext='txt')
# getspec3(ext='txt')
