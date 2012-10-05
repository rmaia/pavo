#Calculates delta R (%) for a set of spectra
diff.refl2<-function(Y,ylim=c(-1,1), main="",select=NULL,lwd=1)

{

final<-NULL
data<-NULL
wl<-c(300:700)

if(length(select)==0)

{
	

for(i in 1:ncol(Y))
	
	{
	
	data<-100*(Y[,i]/Y[,1]-1)
	final<-cbind(final,data)
	
	}

#Plots
colors<-rainbow(ncol(Y),alpha=.9)
plot(final[,1]~wl,type='l',ylim=ylim,ylab=expression(paste(Delta,"R (%)")),xlab="Wavelength (nm)",main=main,lwd=lwd)

for(i in 2:ncol(Y))

{
	
	lines(final[,i]~wl,col=colors[i-1],lwd=lwd)
	
	}

}

else

{
	
	for(i in select)

	{

	data<-100*(Y[,i]/Y[,select[1]]-1)
	final<-cbind(final,data)

	}

#Plots
colors<-rainbow(ncol(final),alpha=.9)
plot(final[,1]~wl,type='l',ylim=ylim,ylab=expression(paste(Delta,"R (%)")),xlab="Wavelength (nm)",main=main,lwd=lwd)

for(i in 2:ncol(final)) 

{

	lines(final[,i]~wl,col=colors[i-1],lwd=lwd)
	
}
}
}