gather.spectra<-function(where,decimal=".")
{
final<-data.frame(wl=300:700)

oldwd<-getwd()

setwd(where)

files<-list.files(pattern=".ttt$")


for(i in files)
{
	
dados<-read.table(i, dec=decimal, sep=";", skip=7, nrows=401)
names(dados)<-c("wavelength",i)

interp<-approx(dados[,1], dados[,2], xout=300:700)$y

interp<-as.data.frame(interp)

names(interp)<-i
names(interp)<-strsplit(names(interp),".ttt")

#SOMAR MINIMO SE FOR MENOR QUE ZERO
#if(min(interp) < 0) {interp<-interp + abs(min(interp))}

final<-data.frame(final, interp)
}

setwd(oldwd)

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