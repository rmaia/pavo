binspecs<-function(Y,bw=20){
num<-seq(1,401,length.out=bw+1)
data<-NULL

#SUBTRACT REF VALUES BY MEAN REFLECTANCE PER CUTHILL ET AL 1999
avg<-apply(Y,2,mean)
Y2<-NULL
for(i in 1:ncol(Y))
	{
	Ynew<-Y[,i]-avg[i]
	Y2<-cbind(Y2,Ynew)
	}
Y2<-as.data.frame(Y2)
names(Y2)=names(Y)

#BINS SPECTRA AS MEDIANS OVER 20-NM BANDWIDTHS
for (i in num[1:bw])
	{
	data2<-apply(Y2[(i:i+diff(num)[1]),],2,median)
	data<-rbind(data,data2)
	}

#data<-subset(data.frame(data,row.names=num),select=-wl)
data<-subset(data.frame(data,row.names=num[1:bw]))

final<-t(data)
#YOU CAN DO A PCA ON THIS FINAL DATA FRAME
final
}
