colordistance<-function(y, x=1, visual="tetra", w1=0.05, w2=0.05, w3=0.05/sqrt(2), w4=0.05/sqrt(2))
{

res<-matrix(NA,nrow(y),nrow(y))

	f1<-as.matrix(y[1])
	f2<-as.matrix(y[2])
	f3<-as.matrix(y[3])
	f4<-as.matrix(y[4])
	
for(i in 1:nrow(f1))
	{
	for(j in 1:nrow(f1))
		{
		res[i,j]<-sqrt((((w1*w2)^2)*(((f4[i]-f4[j])-(f3[i]-f3[j]))^2)+((w1*w3)^2)*(((f4[i]-f4[j])-(f2[i]-f2[j]))^2)+((w1*w4)^2)*(((f3[i]-f3[j])-(f2[i]-f2[j]))^2)+((w2*w3)^2)*(((f4[i]-f4[j])-(f1[i]-f1[j]))^2)+((w2*w4)^2)*(((f3[i]-f3[j])-(f1[i]-f1[j]))^2)+((w3*w4)^2)*(((f2[i]-f2[j])-(f1[i]-f1[j]))^2))/(((w1*w2*w3)^2)+((w1*w2*w4)^2)+((w1*w3*w4)^2)+((w2*w3*w4)^2)))
		
		}
	}

distances<-data.frame(res,row.names=rownames(y))
as.dist(distances)
}




acrodistance<-function(y, x=1, visual="tetra", w=0.05)
{

res<-matrix(NA,nrow(y),nrow(y))


	f<-as.matrix(y[5])
	
for(i in 1:nrow(f))
	{
	for(j in 1:nrow(f))
		{
		res[i,j]<-(f[i]-f[j]/w)
		
		}
	}

distances<-data.frame(res,row.names=rownames(y))
as.dist(distances)
}