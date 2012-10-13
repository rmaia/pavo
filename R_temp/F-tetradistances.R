tetradistance<-function(y, n1=1, n2=2, n3=2, n4=4, v=0.1)
{

w1=v/sqrt(n1)
w2=v/sqrt(n2)
w3=v/sqrt(n3)
w4=v/sqrt(n4)


f1<-as.matrix(y$u)
f2<-as.matrix(y$s)
f3<-as.matrix(y$m)
f4<-as.matrix(y$l)

res<-matrix(NA, nrow = nrow(f1), ncol = nrow(f1))

for(i in 1:nrow(f1))
	{
	for(j in 1:nrow(f1))
		{
		dq1<-log(f1[i]/f1[j],base=10)
		dq2<-log(f2[i]/f2[j],base=10)
		dq3<-log(f3[i]/f3[j],base=10)
		dq4<-log(f4[i]/f4[j],base=10)
		
		numer<-	((w1*w2)^2)*((dq4-dq3)^2) + 
				((w1*w3)^2)*((dq4-dq2)^2) +
				((w1*w4)^2)*((dq3-dq2)^2) +
				((w2*w3)^2)*((dq4-dq1)^2) +
				((w2*w4)^2)*((dq3-dq1)^2) +
				((w3*w4)^2)*((dq2-dq1)^2)
		
		denom<- ((w1*w2*w3)^2) +
				((w1*w2*w4)^2) + 
				((w1*w3*w4)^2) +
				((w2*w3*w4)^2)	
			
		res[i,j]<-sqrt(numer/denom)
	
		}
	}

distances<-data.frame(res,row.names=rownames(y$fi))
as.dist(distances)
}