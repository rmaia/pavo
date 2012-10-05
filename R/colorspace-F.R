colorspace<- function (u,s=0,m=0,l=0)
{

if(is.data.frame(u)==TRUE | is.matrix(u)==TRUE){

data<-u/apply(u,1,sum)
u<-data$Qu
s<-data$Qs
m<-data$Qm
l<-data$Ql
}

#DATA POINT CARTESIAN COORDINATES
x<- ((1-2*s-m-u)/2)*sqrt(3/2)
y<- (-1+3*m+u)/(2*sqrt(2))
z<- u-(1/4)

#VERTEX CARTESIAN COORDINATES
xu<- ((1-2*0-0-1)/2)*sqrt(3/2)
yu<- (-1+3*0+1)/(2*sqrt(2))
zu<- 1-(1/4)

xs<- ((1-2*1-0-0)/2)*sqrt(3/2)
ys<- (-1+3*0+0)/(2*sqrt(2))
zs<- 0-(1/4)

xm<- ((1-2*0-1-0)/2)*sqrt(3/2)
ym<- (-1+3*1+0)/(2*sqrt(2))
zm<- 0-(1/4)

xl<- ((1-2*0-0-0)/2)*sqrt(3/2)
yl<- (-1+3*0+0)/(2*sqrt(2))
zl<- 0-(1/4)

#DATA POINT SPHERICAL COORDINATES
r.vector<- sqrt(x*x + y*y + z*z)
if(any(r.vector == 0))
return(NaN)


Hue.theta<- atan2(y,x)
Hue.phi<- asin(z/r.vector)

#Rmax & Robtained

rvector.u<- sqrt(xu*xu + yu*yu + zu*zu)
Huetheta.u<- atan2(yu,xu)
Huephi.u<- asin(zu/rvector.u)

rvector.s<- sqrt(xs*xs + ys*ys + zs*zs)
Huetheta.s<- atan2(ys,xs)
Huephi.s<- asin(zs/rvector.s)

rvector.m<- sqrt(xm*xm + ym*ym + zm*zm)
Huetheta.m<- atan2(ym,xm)
Huephi.m<- asin(zm/rvector.m)

rvector.l<- sqrt(xl*xl + yl*yl + zl*zl)
Huetheta.l<- atan2(yl,xl)
Huephi.l<- asin(zl/rvector.l)

cosalfa.u<-cos(Hue.phi)*cos(Huephi.u)*cos(Hue.theta-Huetheta.u)+sin(Hue.phi)*sin(Huephi.u)
cosalfa.s<-cos(Hue.phi)*cos(Huephi.s)*cos(Hue.theta-Huetheta.s)+sin(Hue.phi)*sin(Huephi.s)
cosalfa.m<-cos(Hue.phi)*cos(Huephi.m)*cos(Hue.theta-Huetheta.m)+sin(Hue.phi)*sin(Huephi.m)
cosalfa.l<-cos(Hue.phi)*cos(Huephi.l)*cos(Hue.theta-Huetheta.l)+sin(Hue.phi)*sin(Huephi.l)

all.cosalfa<-data.frame(cosalfa.u,cosalfa.s,cosalfa.m,cosalfa.l)

cos.alfamax<-as.numeric(lapply(data.frame(t(all.cosalfa)),min))
r.max<- (0.25)/(-(cos.alfamax))

r.achieved<-r.vector/r.max

#CONE STIMULATION
u.r<-r.vector*cosalfa.u
s.r<-r.vector*cosalfa.s
m.r<-r.vector*cosalfa.m
l.r<-r.vector*cosalfa.l

if(is.data.frame(data)==TRUE){
#RESULT
sensitivity<-data.frame(u,s,m,l,row.names=rownames(data))
stimulation<-data.frame(u.r,s.r,m.r,l.r,row.names=rownames(data))
cartesian<-data.frame(x,y,z,row.names=rownames(data))
spherical<-data.frame(Hue.theta,Hue.phi,r.vector,r.max,r.achieved,row.names=rownames(data))

return(list(sensitivity = sensitivity, relative.stimulation = stimulation, cartesian = cartesian, spherical = spherical))

}else
{
sensitivity<-data.frame(u,s,m,l)
stimulation<-data.frame(u.r,s.r,m.r,l.r)
cartesian<-data.frame(x,y,z)
spherical<-data.frame(Hue.theta,Hue.phi,r.vector,r.max,r.achieved)
}

return(list(sensitivity = sensitivity, relative.stimulation = stimulation, cartesian = cartesian, spherical = spherical))
}