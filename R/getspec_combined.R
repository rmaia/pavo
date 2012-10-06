# function to get all specs from a given folder
# currently works with USB2000, USB4000, jaz & CRAIC (exported)

#clumsy: if subdir=T, column name includes subdir name (desired?)

getspec<-function(where, ext='txt', decimal=".", subdir=F)
{

extension <- paste('.', ext, sep='')

file_names <- list.files(where, pattern=extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',file_names,sep='')

if(length(file_names)==0){
	stop('No files found. Try a different ext')
	} 

#ToDo change this part if we want to make it wavelength-flexible:

final <- data.frame(matrix(nrow=401, ncol=length(file_names)+1))
final[,1] <- 300:700

#until here

for(i in 1:length(files))
{

raw <- scan(file=files[i], what='', quiet=T, dec=decimal, sep='\n')
#ToDo we can actually use this raw string to import metadata if we want

start <- grep('\t',raw)[1] - 1
end <- length(grep('\t',raw))

#jaz output file is weird. has 5 columns and an extra line in bottom

if(extension=='.jaz'){
	tempframe <- read.table(files[i], dec=decimal, skip=start, nrows=end-1, header=T)
	tempframe <- tempframe[c('W','P')]
	}else{
tempframe <- read.table(files[i], dec=decimal, skip=start, nrows=(end-start-1))		
	}

#ToDo make wavelength-flexible
interp<-data.frame(approx(tempframe[,1], tempframe[,2], xout=300:700))
names(interp) <- c("wavelength", strsplit(file_names[i], extension) )

#SpectraSuite sometimes allows negative values. Remove those:
if(min(interp[,2]) < 0) {interp[,2]<-interp[,2] + abs(min(interp[,2]))}

final[,i+1] <- interp[,2]

}

names(final) <- c('wl',gsub(extension,'',file_names))
final
}




mean.spectra<-function(gatherspectra,BY)
{
dup<-seq(2,length(names(gatherspectra)),by=BY)

samplenames<-names(gatherspectra[dup])

avgZ.i<-NULL
avgZ<-NULL

for(i in dup)
	{
	
	avgZ.i<-apply(gatherspectra[i:(i+BY-1)],1,mean)
	avgZ<-cbind(avgZ,avgZ.i)
	
	}

avgZ<-data.frame(avgZ)
names(avgZ)<-samplenames
avgZ
}	