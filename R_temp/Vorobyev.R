Vorobyev<-function(y,visual="star", relative=T, iluminant=default.iluminant,background=default.background)
{
sens<-read.table("/Users/RMaia/Desktop/Projects/TetraColoR/visual-systems.txt", header=T)

if(is.data.frame(y)==FALSE){y<-data.frame(y)}

#DEFINING ILUMINANT & BACKGROUND
default.iluminant<-rep(1,401)
default.background<-rep(1,401)

#BRIGHTNESS
norm.brilliance<-data.frame(norm.B=apply(y,2,sum)/(40100))
max.brightness<-data.frame(max.B=apply(y,2,max))

#WAVELENGTH OF MAXIMUM REFLECTION
lambda.max<-function(G)
  {
  lambdas<-NULL
  wl<-300:700
  for(i in 1:ncol(G))
     { 
     lambdalist<- wl[G[,i]==max(G[,i])]
     lambdalist<-max(lambdalist)
     lambdas<-c(lambdas,lambdalist)
     }

  lambdas
  }
lambda.max<-lambda.max(y)

#NORMALIZATION
sums<-apply(y,2,max) #VERIFICAR SE É ESSA ESCALAÇÃO
x<-scale(y, center=F, scale=sums)
x<-data.frame(x)

#SEM NORMALIZAR
#x<-y/100 #só transformando em proporção, ao invés de porcentabem (assim 100% reflectance = 1.0 )

#REMOVER VALORES NEGATIVOS
  for(i in 1:ncol(x))
     { 
     if(min(x[,i])<0){x[,i]<-x[,i]+abs(min(x[,i]))}
          }


#Qi
if(visual=="avg.uv")
        {
          S.u<-sens$avg.uv.u
          S.s<-sens$avg.uv.s
          S.m<-sens$avg.uv.m
          S.l<-sens$avg.uv.l
        } else{
        	if(visual=="avg.v")
        {
          S.u<-sens$avg.v.v
          S.s<-sens$avg.v.s
          S.m<-sens$avg.v.m
          S.l<-sens$avg.v.l
        } else{
        	if(visual=="BT")
        {
          S.u<-sens$BT.u
          S.s<-sens$BT.s
          S.m<-sens$BT.m
          S.l<-sens$BT.l
        } else{
        	if(visual=="star")
        {
          S.u<-sens$star.u
          S.s<-sens$star.s
          S.m<-sens$star.m
          S.l<-sens$star.l
        } else{
        	if(visual=="pfowl")
        {
          S.u<-sens$pfowl.v
          S.s<-sens$pfowl.s
          S.m<-sens$pfowl.m
          S.l<-sens$pfowl.l
        } else{stop("Invalid sensory type: visual= must be avg.uv, avg.v, BT, star or pfowl")}
        	}
        	}
        }
        	}

      
                  
if(length(x[,1])==401)
  {
          Qu.notnorm<-apply(x*S.u*iluminant,2,sum)
          Qs.notnorm<-apply(x*S.s*iluminant,2,sum)
          Qm.notnorm<-apply(x*S.m*iluminant,2,sum)
          Ql.notnorm<-apply(x*S.l*iluminant,2,sum)
          
          Qi.notnorm<-data.frame(Qu.notnorm,Qs.notnorm,Qm.notnorm, Ql.notnorm)
          Qi.notnorm.sum<-apply(Qi.notnorm,1,sum)
          
         if(relative==T){ 
         	Qi<-Qi.notnorm/Qi.notnorm.sum
         	blacks <- which(norm.brilliance < 0.05) #find dark specs
         	Qi[blacks,] <- 0.2500 #place dark specs in achromatic center
         	 } else {Qi<-Qi.notnorm}
        
         
names(Qi)<-c("Qu","Qs","Qm","Ql")
          
          
  }else
    {stop("Data spectra must be 1-nm intervals, within 300:700 nm, with 401 cells each")}


#qi # VER NOME DAS LINHAS
k.u<- 1/(sum(S.u*background*iluminant))
k.s<- 1/(sum(S.s*background*iluminant))
k.m<- 1/(sum(S.m*background*iluminant))
k.l<- 1/(sum(S.l*background*iluminant))

qu<- k.u*Qi$Qu
qs<- k.s*Qi$Qs
qm<- k.m*Qi$Qm
ql<- k.l*Qi$Ql

#fi
fu<-log(qu)
fs<-log(qs)
fm<-log(qm)
fl<-log(ql)

#OUTPUT

descriptive<-data.frame(norm.brilliance,max.brightness, lambda.max)

qi<-data.frame(qu,qs,qm,ql,row.names=row.names(Qi))

fi<-data.frame(fu,fs,fm,fl,row.names=row.names(Qi))

if(visual=="marmoset")
	{
		names(Qi)<-c("Q.425","Q.543","Q.556","Q.561")
		names(qi)<-c("q.425","q.543","q.556","q.561")
		names(fi)<-c("f.425","f.543","f.556","f.561")
	}
          	else{names(Qi)<-c("Qu","Qs","Qm","Ql")}

return(list(descriptive=descriptive,Qi=Qi, qi=qi, fi=fi))
}