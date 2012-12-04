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
#' getspec('/examplespec', lim=c(400,900))	
#' getspec('/examplespec', ext='ttt')}
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

getspec <- function(where=getwd(), ext='txt', lim=c(300,700), decimal=".", 
           subdir=FALSE, subdir.names=FALSE)
{

negative <- match.arg(negative)
makenegzero<-function(x) {x[x[,2]<0,2] = 0; x}

separ=ifelse(ext=='ttt',';','\t')

extension <- paste('.', ext, sep='')

file_names <- list.files(where, pattern=extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',file_names,sep='')

if(subdir.names){
	file_names <- gsub(extension,'',file_names)}else{
	file_names <- gsub(extension,'',basename(file_names))
	 }

if(length(file_names)==0){
	stop('No files found. Try a different ext')
	} 

range <- lim[1]:lim[2]

final <- data.frame(matrix(nrow=length(range), ncol=length(file_names)+1))
final[,1] <- range


for(i in 1:length(files))
{

raw <- scan(file=files[i], what='', quiet=T, dec=decimal, sep='\n')
#ToDo we can actually use this raw string to import metadata if we want

start <- grep(separ,raw)[1] - 1
end <- length(grep(separ,raw))

#Avantes ttt files don't use tab-delimiting, but semicolon-delimiting
#also has two lines with semicolon that are not data

if(extension=='.ttt'){
	start <- grep(separ,raw)[3] -1
	end <- length(grep(separ,raw)) + start -1
}

#jaz output file is weird. has 5 columns and an extra line in bottom

if(extension=='.jaz'){
	tempframe <- read.table(files[i], dec=decimal, sep=separ, skip=start, nrows=end-1, 
							header=T)
	tempframe <- tempframe[c('W','P')]
	}else{
tempframe <- read.table(files[i], dec=decimal, sep=separ, skip=start, nrows=(end-start-1))		
	}

interp<-data.frame(approx(tempframe[,1], tempframe[,2], xout=range))
names(interp) <- c("wavelength", strsplit(file_names[i], extension) )

final[,i+1] <- interp[,2]

}

names(final) <- c('wl',gsub(extension,'',file_names))
class(final) <- c('rspec','data.frame')
	
final
}