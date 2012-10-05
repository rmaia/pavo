#spectrum --> rgb
spec2rgb<-function(Y2)

{

sens<-read.csv("/Users/RMaia/Desktop/Projects/TetraColoR/spec2rgb/spec.csv")

P1<-Y2-min(Y2)
P2<-P1/max(P1)

X<-sum(sens[1:401,10]*P2)

Y<-sum(sens[1:401,11]*P2)

Z<-sum(sens[1:401,9]*P2)

xyzmat<-matrix(c(3.240479,-0.969256,0.055648,-1.537150,1.875992,-0.204043,-0.498535,0.041556,1.057311),nrow=3)

rgb<-xyzmat%*%matrix(c(X,Y,Z)/sum(X,Y,Z),ncol=1)
rgb[rgb<0]<-0
rgb[rgb>1]<-1

rgb=t(rgb)


}