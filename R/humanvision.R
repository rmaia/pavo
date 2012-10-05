humanvis=function(x)
{

	x=x/580

          Qs.notnorm<-apply(x*human$S,2,sum)
          Qm.notnorm<-apply(x*human$M,2,sum)
          Ql.notnorm<-apply(x*human$L,2,sum)	
	
	Qi<-data.frame(Qs.notnorm,Qm.notnorm, Ql.notnorm)
	names(Qi)<-c("Qs","Qm","Ql")

	Qi
	
	}






tridistance<-function(y, n1=1, n2=2, n3=2, v=0.05)
{

#w1=v/sqrt(n1)
#w2=v/sqrt(n2)
#w3=v/sqrt(n3)

w1=0.08/sqrt(1)
w2=0.02/sqrt(1.8)
w3=0.02/sqrt(2)

dados<-y
sums<-apply(dados,1,sum)
dados<-dados/sums

f1<-as.matrix(dados$Qs)
f2<-as.matrix(dados$Qm)
f3<-as.matrix(dados$Ql)

res<-matrix(NA,nrow(f1),nrow(f1))

for(i in 1:nrow(f1))
	{
	for(j in 1:nrow(f1))
		{
		dq1<-log(f1[i]/f1[j],base=10)
		dq2<-log(f2[i]/f2[j],base=10)
		dq3<-log(f3[i]/f3[j],base=10)
		
		numer<-(w1^2*(dq2-dq1)^2)+(w2^2*(dq3-dq1)^2)+(w3^2*(dq1-dq2)^2)
		
		denom<-((w1*w2)^2)+((w1*w3)^2)+((w2*w3)^2)	
			
		res[i,j]<-sqrt(numer/denom)
	
		}
	}

distances<-data.frame(res,row.names=rownames(y))
as.dist(distances)
}


humanvis(cbind(lepi[-1],branco))
Vorobyev(cbind(lepi[-1],branco),visual="avg.v")$Qi
tridistance(humanvis(cbind(lepi[-1],branco)))
tetradistance(Vorobyev(cbind(lepi[-1],branco),visual="avg.v")$Qi,n1=1,n2=1.9,n3=2.2,n4=2.1)



barplot(medias,col=rep(c("grey","white"),4),space=rep(c(1,0),4),ylim=c(0,15),ylab="Contrast (delta S)")
axis(1,at=c(2,5,8,11),labels=c("NC","NR","SC","SR"))

segments(c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),medias+erros,c(1.5,2.5,4.5,5.5,7.5,8.5,10.5,11.5),medias-erros)
abline(h=1,lty=3)

legend("topleft",c("Human","Avian"),fill=c("grey","white"),bty="n")