# function to get all specs from a given folder
# allows for getting specs from subdir - but the column name will include the subdir (fix)
# currently works with USB2000, USB4000, jaz, CRAIC (exported) & Avantes (exported)

#clumsy: if subdir=T, column name includes subdir name (desired?)

# CME: can we have this automatically set the lower and upper limits based on the spec files? 
#      These files are usually a pain to view in a text editor to see what wl range was used.
# RM: hm.. not sure I like this idea very much. two problems: (1) if specs have different ranges
#     they can't be added to the same matrix without adding NAs; (2) If saved spec includes
#     a range that the spec can't actually read, it will be very noisy and may mess corrections
#     (depending on how these are handled).
#   suggested solution: a secondary function that examines files and returns recorded WL range
#     (in a dataframe or table)

getspec<-function(where, ext='txt', lim=c(300,700), decimal=".", subdir=F)
{

separ=ifelse(ext=='ttt',';','\t')

extension <- paste('.', ext, sep='')

file_names <- list.files(where, pattern=extension, recursive=subdir, include.dirs=subdir)
files <- paste(where,'/',file_names,sep='')

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
	tempframe <- read.table(files[i], dec=decimal, sep=separ, skip=start, nrows=end-1, header=T)
	tempframe <- tempframe[c('W','P')]
	}else{
tempframe <- read.table(files[i], dec=decimal, sep=separ, skip=start, nrows=(end-start-1))		
	}

interp<-data.frame(approx(tempframe[,1], tempframe[,2], xout=range))
names(interp) <- c("wavelength", strsplit(file_names[i], extension) )

#SpectraSuite sometimes allows negative values. Remove those:

# CME: I don't know if this is the right way to go about this. If some specs have neg values while 
#      others don't their relative brightnesses will be meaningless. Maybe replace with NAs or zeros?
# RM: ToDo: include switch for option as to how to change this
#     (add min or zero; not NA - trickles down other functions that can't handle)

if(min(interp[,2], na.rm=T) < 0) {interp[,2]<-interp[,2] + abs(min(interp[,2], na.rm=T))}

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