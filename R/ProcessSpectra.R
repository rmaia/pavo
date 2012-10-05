#MAXIMIZE SPECTRA TO MAX = 1
max.spec<-function(Y)
{
	final<-NULL
	for(i in 1:ncol(Y))
	{
		specs<-Y[,i]/max(Y[,i])
		final<-cbind(final,specs)
		}
		final
		}

#SMOOTH SPECTRA
smooth.spec<-function(Y,span=.1,degree=1)

{
	xx<-NULL
	
	for(i in 2:ncol(Y)) {
		
		xx2<-loess.smooth(Y[,1],Y[,i],span=span,degree=degree,evaluation=401)$y
		xx<-cbind(xx,xx2)
		
		}

final<-data.frame(xx)
names(final)=names(Y)[-1]
#[-1] is if wl is first column of Y data frame
final

}

#NORMALIZE SPECTRA TO MIN = 0
min.spec<-function(Y)

{
	
	mins<-apply(Y,2,min)
	final<-NULL
	
	for(i in 1:ncol(Y))
	{
		spec<-Y[,i]-mins[i]
		final<-cbind(final,spec)
	}		

final				

}

#NORMALIZE SPECTRA TO SUM = 1
sum.spec<-function(Y)
{
	final<-NULL
	for(i in 1:ncol(Y))
	{
		data<-Y[,i]/sum(Y[,i])
		final<-cbind(final,data)
		
		}
final
}

#CALC DIFF SPECTRA FROM BASELINE
diff.spec<-function(Y)

{

specs.diff<-NULL
spec<-NULL

for(i in 1:ncol(Y))
	{
	spec<-((Y[,i]-Y[,1])/Y[,1])
	specs.diff<-cbind(specs.diff,spec)
	}
	
specs.diff

}