colorsum<- function(cores)
{
#CENTROID
centroid<-data.frame(x=mean(cores$cartesian$x),y=mean(cores$cartesian$y),z=mean(cores$cartesian$z))

#COLOR SPAN
mean.colorspan<- mean(dist(cores$cartesian))
var.colorspan<- var(dist(cores$cartesian))
Color.span<-data.frame(mean=mean.colorspan,variance=var.colorspan)

#COLOR VOLUME
colormatrix<-as.matrix(data.frame(cores$cartesian$x,cores$cartesian$y,cores$cartesian$z))
if(nrow(colormatrix)>3)
     {
     Color.vol<-as.vector(convhulln(colormatrix,"FA")$vol)
     }else
      {Color.vol<-NA}

#HUE DISPARITY
Hue.disp <- function(cores) 
{
   a<-as.matrix(cores$spherical$Hue.phi)
   b<-as.matrix(cores$spherical$Hue.theta)

      res <- matrix(NA,nrow(a),nrow(b))

      for (i in 1:nrow(a)) {
          for (j in 1:nrow(a)) {
              res[i,j] <- acos((cos(a[i])*cos(a[j])*cos(b[i]-b[j]))+(sin(a[i])*sin(a[j])))

        }
    }
mean.HD<-mean(res[lower.tri(res)==TRUE])
var.HD<-var(res[lower.tri(res)==TRUE])
table.huedisp<-data.frame(mean=mean.HD,variance=var.HD)
table.huedisp
}

Hue.disparity<-Hue.disp(cores)

#ACHIEVED CHROMA
mean.ra<-mean(cores$spherical$r.achieved)
max.ra<-max(cores$spherical$r.achieved)
Achieved.chroma<-data.frame(mean.ra=mean.ra,maximum.ra=max.ra)

#WARNINGS
ifelse(nrow(colormatrix)<3,warning("not enough color points to calculate variances (min. 3)",call.=F),noodles<-0)
ifelse(nrow(colormatrix)<4,warning("not enough color points to calculate volume (min. 4)",call.=F),noodles<-0)

#RESULTS
return(list(Centroid=centroid,Color.span=Color.span,Color.volume=Color.vol,Hue.disparity=Hue.disparity,Achieved.chroma=Achieved.chroma))
}
