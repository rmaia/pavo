require(mapproj)

projplot=function(x)
{
#extract relevant sperical coordinates
 # points.theta=x$spherical$Hue.theta
 # points.phi=x$spherical$Hue.phi
 # size=x$spherical$r.achieved

points.theta=x$hue.theta
points.phi=x$hue.phi


n=length(points.theta)

#Edges of the tetrahedron, adjusted
vert.theta=c(-3.141593, -1.047198, 1.047198, -2.617994)
vert.phi=c(-0.3398369, -0.3398369, -0.3398369,  1.5707963)

#Edges of the figure
edge.theta=c(-pi,-pi,pi,pi)
edge.phi=c(-pi/2,pi/2,-pi/2,pi/2)

#adjust points
for(i in 1:n){ if(points.theta[i]>= -0.5235988){points.theta[i]=points.theta[i]-(150/180*pi)}else{points.theta[i]=points.theta[i]+(210/180*pi)}}

#map projection coordinates
mp=mapproject(c(edge.theta,vert.theta,points.theta),c(edge.phi,vert.phi,points.phi),projection="cylindrical")

mp.v.theta=mp$x[1:8]
mp.v.phi=mp$y[1:8]

mp.p.theta=mp$x[-c(1:8)]
mp.p.phi=mp$y[-c(1:8)]

#plot
 plot(mp.v.phi~mp.v.theta,pch=4,cex=2,col=c(rep('white',4),'red','green','blue','violet'),axes=F,xlab='',ylab='',frame.plot=T)

 points(mp.p.phi~mp.p.theta,pch=20,cex=5*size,col=mela$x)
 points(mp.p.phi~mp.p.theta,pch=20,col=mela$x)
}

#projplot(colorspace(star))
projplot(ducks)