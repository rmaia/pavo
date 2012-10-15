

tcs<- function(vismodeldata, qcatch=c('Qi','qi','fi'))
{

if(is.list(vismodeldata)){
	qcatch <- match.arg(qcatch)
	dat <- data.frame(vismodeldata[qcatch])
  }
  
# make relative (in case not inherited relative)

dat <- dat/rowSums(dat)

u <- dat[,1]
s <- dat[,2]
m <- dat[,3]
l <- dat[,4]

# cartesian coordinates

x <- ((1-2*s-m-u)/2)*sqrt(3/2)
y <- (-1+3*m+u)/(2*sqrt(2))
z <- u-(1/4)

# vertex cartesian coordinates & their spherical data

ttvx <- pavo::ttvertex

# spherical coordinates for the data points

r.vec<- sqrt(x*x + y*y + z*z)
r.vec[r.vec==0] = NaN


h.theta<- atan2(y,x)
h.phi<- asin(z/r.vec)

#Rmax & Robtained

cosalpha.u <- cos(h.phi)*cos(ttvx$Huephi.u)*cos(h.theta-ttvx$Huetheta.u) +
  sin(h.phi)*sin(ttvx$Huephi.u)
cosalpha.s <- cos(h.phi)*cos(ttvx$Huephi.s)*cos(h.theta-ttvx$Huetheta.s) +
  sin(h.phi)*sin(ttvx$Huephi.s)
cosalpha.m <- cos(h.phi)*cos(ttvx$Huephi.m)*cos(h.theta-ttvx$Huetheta.m) +
  sin(h.phi)*sin(ttvx$Huephi.m)
cosalpha.l <- cos(h.phi)*cos(ttvx$Huephi.l)*cos(h.theta-ttvx$Huetheta.l) +
  sin(h.phi)*sin(ttvx$Huephi.l)

allcosalpha <- data.frame(cosalpha.u,cosalpha.s,cosalpha.m,cosalpha.l)

cosalphamax <- apply(allcosalpha,1,min)
r.max<- (0.25)/(-(cosalphamax))

r.achieved <- r.vec/r.max

# cone stimulation (for a given hue as a function of saturation, see S&P ESM)
# this is not really used, ever -- should we include it?

u.r<-r.vec*cosalpha.u
s.r<-r.vec*cosalpha.s
m.r<-r.vec*cosalpha.m
l.r<-r.vec*cosalpha.l

res <- data.frame(u, s, m, l, u.r , s.r, m.r, l.r, 
                  x, y, z, h.theta, h.phi, 
                  r.vec, r.max, r.achieved,
                  row.names=rownames(dat))
#names(res)[1:4] <- names(dat)

res
}