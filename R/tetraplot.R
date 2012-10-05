tetraplot<-function(Y,angle=65,scale.y=0.45)

{

par(mar=c(1,1,1,1))

test<-scatterplot3d(x=Y$x,y=Y$y,z=Y$z,box=T,pch=16,color=rgb(0,0,0,.5),xlim=c(-1.22,.612),ylim=c(-.35,.707),zlim=c(-.25,.75),axis=F,grid=F,angle=angle,scale.y=scale.y,cex.symbols=1,mar=c(1,1,1,1))

#test$points3d(hofi2$x,hofi2$y,hofi2$z,col=rgb(0,0,1,.5),pch=16)
#test$points3d(x=eabl$x,y=eabl$y,z=eabl$z,col=rgb(0,0,1,.5),pch=16,cex=1)

#test$points3d(x=puma$x,y=puma$y,z=puma$z,col=rgb(1,0,1,.5),pch=16,cex=1)

#segments(1.11,4.639,-2.30,-1.14)
#segments(-2.3,-1.14,3.075,1.21,lty=2)
#segments(3.075,1.21,1.362889,-1.13889)
#segments(1.11,4.639,3.075,1.21)
#segments(1.11,4.639,1.362889,-1.13889)
#segments(-2.301,-1.1389,1.362889,-1.13889)

uv<-test$xyz.convert(0,0,.75)
blue<-test$xyz.convert((-1*sqrt(1.5)),(-1/(2*sqrt(2))),-.25)
green<-test$xyz.convert(0,(1/sqrt(2)),-.25)
red<-test$xyz.convert((.5*sqrt(1.5)),(-1/(2*sqrt(2))),-.25)
no.uv<-test$xyz.convert(0,0,-.25)

segments(uv$x,uv$y,red$x,red$y)
segments(uv$x,uv$y,green$x,green$y)
segments(uv$x,uv$y,blue$x,blue$y)
segments(red$x,red$y,green$x,green$y)
segments(green$x,green$y,blue$x,blue$y)
segments(blue$x,blue$y,red$x,red$y)
#segments(uv$x,uv$y,no.uv$x,no.uv$y)

#polygon(x=c(blue$x,red$x,green$x,blue$x),y=c(blue$y,red$y,green$y,blue$y),col=rgb(0,0,0,.02))
#polygon(x=c(blue$x,uv$x,red$x),y=c(blue$y,uv$y,red$y),col=rgb(0,0,0,.02))
#polygon(x=c(blue$x,green$x,uv$x),y=c(blue$y,green$y,uv$y),col=rgb(0,0,0,.02))

text(red$x,red$y,"red")
text(green$x,green$y,"green")
text(blue$x,blue$y,"blue")
text(uv$x,uv$y,"uv")

}