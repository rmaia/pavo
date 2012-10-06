
#as is, takes exactly the same amount of time

parsespec <- function(x, argum=args){
	
raw <- scan(file=x, what='', quiet=T, dec=argum$decim, sep='\n')
#ToDo we can actually use this raw string to import metadata if we want

start <- grep('Begin', raw)
end <- grep('End', raw)

#jaz output file is weird. has 5 columns and an extra line in bottom

if(argum$extension=='.jaz'){
	tempframe <- read.table(x, dec=argum$decim, skip=start, nrows=(end-start-2), header=T)
	tempframe <- tempframe[c('W','P')]
	}else{
tempframe <- read.table(x, dec=argum$decim, skip=start, nrows=(end-start-1))		
	}

#ToDo make wavelength-flexible
interp<-data.frame(approx(tempframe[,1], tempframe[,2], xout=300:700))

#SpectraSuite sometimes allows negative values. Remove those:
if(min(interp[,2]) < 0) {interp[,2]<-interp[,2] + abs(min(interp[,2]))}

interp[,2]
	
}

# function to get all specs from a given folder
# currently works with USB2000 & USB4000 & jaz

#clumsy: if subdir=T, column name includes subdir name (desired?)

getspec_vector<-function(where, filetype='ProcSpec', decim=".", subdir=F)
{

#final<-data.frame(wl=300:700)
args <- vector('list')

args$extension <- paste('.', filetype, sep='')

args$file_names <- list.files(where, pattern=args$extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',args$file_names,sep='')

args$format <- filetype
args$decim <- '.'
args$where <- where

if(length(args$file_names)==0){
	stop('No files found. Try a different format')
	} 

files <- as.list(files)

result <- data.frame(lapply(files,function(x) parsespec(x,argum=args)))

names(result) <- gsub(args$extension,'',args$file_names)

result

}


igh = "~/Downloads/szymek.drobniak@uj.edu.pl - Re: Other: jaz ocean optics spectrophotometer - question"

head(getspec(igh, filetype='jaz'))