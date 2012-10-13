i.stimulus<-function(y,visual="v")
{
sens<-read.table("sensitivityn.txt", header=T)  

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
     lambdas<-c(lambdas,lambdalist)
     }

  lambdas
  }
lambda.max<-lambda.max(y)

#NORMALIZATION
sums<-apply(y,2,max)
x<-scale(y, center=F, scale=sums)
x<-data.frame(x)

#SENSITIVITY   

size<-ncol(x)
for(i in 1:size)
{
if(length(x[,i])==401)
  {
   if(visual=="uv" | visual=="v")
     {
      if(visual=="uv")
        {
          u<-apply(x*sens$uv.u,2,sum)
          s<-apply(x*sens$uv.s,2,sum)
          m<-apply(x*sens$uv.m,2,sum)
          l<-apply(x*sens$uv.l,2,sum)
          usml<-data.frame(u,s,m,l)
          sumout<-apply(usml,1,sum)
          usml.norm<-usml/sumout
          u<-u/sumout
          s<-s/sumout
          m<-m/sumout
          l<-l/sumout
      }else
        {
          u<-apply(x*sens$v.u,2,sum)
          s<-apply(x*sens$v.s,2,sum)
          m<-apply(x*sens$v.m,2,sum)
          l<-apply(x*sens$v.l,2,sum)
          usml<-data.frame(u,s,m,l)
          sumout<-apply(usml,1,sum)
          usml.norm<-usml/sumout
          u<-u/sumout
          s<-s/sumout
          m<-m/sumout
          l<-l/sumout
        }
    }else
      {stop("Invalid sensory type - must be uv or v")}
}else
  {stop("Data spectra must be 1-nm intervals, within 300:700 nm, with 401 cells each")}

dadossens<-data.frame(u,s,m,l,norm.brilliance,max.brightness, lambda.max)
}

return(dadossens)
}