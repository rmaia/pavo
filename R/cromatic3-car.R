# calculate chromatic variables from spectral files
# Function written by Rafael Maia, May 2008
# Last updated January 2012

cromatic<-function(G)
{

#HUE
if(length(G[,1])==381)
{

hue<-function(G)
  {
  lambda<-NULL
  wl<-320:700
  for(i in 1:ncol(G))
     { 
     lambdalist<- wl[G[,i]==max(G[,i])]
     lambda<-c(lambda,lambdalist)
     }

  lambda
  }

hue2<-function(G)
  {
  lambda2<-NULL
  wl<-320:700
  for(i in 1:ncol(G))
     { 
     lambdamax<-wl[G[,i]==max(G[,i])]
     lambdamin<-wl[G[,i]==min(G[,i])]
     lambdalist2<- lambdamin+((lambdamax-lambdamin)/2)
     lambda2<-c(lambda2,lambdalist2)
     }

  lambda2
  }

#BRILLIANCE
integral<-apply(G,2,sum)
norm.B<-integral/(38100)

#INTENSITY & CONTRAST
max.B<-apply(G,2,max)
min.B<-apply(G,2,min)
contrast<-max.B-min.B

#CHROMA UV
sumUV<-apply(G[1:81,],2,sum)
C.UV<-sumUV/integral

#CHROMA BLUE
sumBlue<-apply(G[81:181,],2,sum)
C.Blue<-sumBlue/integral

#CAROTENOID CHROMA
Carot.C<-apply(G,2,function(x) (x[131]-x[381])/x[381])

#RED CHROMA
sumred<-apply(G[306:381,],2,sum)
Red.C<-sumred/integral

#YELLOW CHROMA
sumyellow<-apply(G[231:306,],2,sum)
Yellow.C<-sumyellow/integral


}else
{
if(length(G[,1])==401)
{

hue<-function(G)
  {
  lambda<-NULL
  wl<-300:700
  for(i in 1:ncol(G))
     { 
     lambdalist<- wl[G[,i]==max(G[,i])]
     lambda<-c(lambda,lambdalist)
     }

  lambda
  }
  
hue2<-function(G)
  {
  lambda2<-NULL
  wl<-300:700
  for(i in 1:ncol(G))
     { 
     lambdamax<-wl[G[,i]==max(G[,i])]
     lambdamin<-wl[G[,i]==min(G[,i])]
     lambdalist2<- lambdamin+((lambdamax-lambdamin)/2)
     lambda2<-c(lambda2,lambdalist2)
     }

  lambda2
  }

#BRILLIANCE
integral<-apply(G,2,sum)
norm.B<-integral/(40100)

#INTENSITY & CONTRAST
max.B<-apply(G,2,max)
min.B<-apply(G,2,min)
contrast<-max.B/min.B

#CHROMA UV
sumUV<-apply(G[1:101,],2,sum)
C.UV<-sumUV/integral

#CHROMA BLUE
sumBlue<-apply(G[101:201,],2,sum)
C.Blue<-sumBlue/integral

#CAROTENOID CHROMA
Carot.C<-apply(G,2,function(x) (x[151]-x[401])/x[401])

#RED CHROMA
sumred<-apply(G[326:401,],2,sum)
Red.C<-sumred/integral

#YELLOW CHROMA
sumyellow<-apply(G[251:326,],2,sum)
Yellow.C<-sumyellow/integral


}else{stop("Not a valid spectra - must be 300:700 or 320:700")}

}
result<-data.frame(B2=norm.B,H1=hue(G),B3=max.B,contrast,UV.S1=C.UV,Blue.S1=C.Blue,Red.S1=Red.C,Yellow.S1=Yellow.C,S9=Carot.C,Carotenoid.Hue=hue2(G))

result
}