jnd2xyz <- function(coldistres, TRI=F, TET=F) {

cdrbackup <- coldistres
	
coldistres <- rbind(attr(coldistres, 'resrefs'), coldistres)

coldistres <- as.matrix(rbind(coldistres[ ,c(1,2,3)], coldistres[ ,c(2,1,3)]))

uniquepatches <-  unique(c(coldistres[,1], coldistres[,2]))

M <- matrix(nrow=length(uniquepatches), ncol=length(uniquepatches))

rownames(M) <- uniquepatches
colnames(M) <- uniquepatches

M[coldistres[,1:2] ] <- coldistres[,3]
M[coldistres[,2:1] ] <- coldistres[,3]

class(M) <- 'numeric'
M[is.na(M)] <- 0

pos3 <- function(d12, d13, d23){
	x3 <- (d13^2 - d23^2 + d12^2)/(2*d12)
	y3 <- rep(0, 2)
	y3sq <- d13^2 - x3^2
	if(y3sq > 0)
	  y3 <- sqrt(y3sq)*c(1,-1)
	
	matrix(c(rep(x3,2),y3), ncol=2, dimnames=list(NULL, c('x','y')))
}

pos4 <- function(d12, d14, d24, d34){
	x4 <- (d14^2 - d24^2 + d12^2)/(2*d12)
	y4 <- ((d14^2 - d34^2 + thirdpointxy['y']^2 + thirdpointxy['x']^2)/(2*thirdpointxy['y'])) -
	      (x4*(thirdpointxy['x']/thirdpointxy['y']))
	z4 <- rep(0, 2)
	z4sq <- d14^2 - x4^2 - y4^2
	if(z4sq > 0)
	  z4 <- sqrt(z4sq)*c(1,-1)
	matrix(c(rep(x4,2), rep(y4,2), z4), ncol=3, dimnames=list(NULL, c('x','y','z')))
}

if(TET){
coords <- matrix(NA, nrow=nrow(M), ncol=3, dimnames=list(row.names(M), c('x','y', 'z')))

reffornames <- grep('refforjnd2xyz', rownames(coords), value=TRUE)

# first point
coords['refforjnd2xyz.acent', ] <- c(0,0,0)
# second point
coords[reffornames[1], ] <- c(M['refforjnd2xyz.acent',reffornames[1]],0,0)
# third point
thirdpointxy <- pos3(M['refforjnd2xyz.acent', reffornames[1]], 
                     M['refforjnd2xyz.acent', reffornames[as.numeric(attr(cdrbackup,'conenumb'))] ], 
                     M[reffornames[1], reffornames[as.numeric(attr(cdrbackup,'conenumb'))] ])[1, ]
coords[reffornames[as.numeric(attr(cdrbackup,'conenumb'))], ] <- c(thirdpointxy, 0)

#fourth point
fourthpointxyz <- pos4(M['refforjnd2xyz.acent', reffornames[1]], 
                       M['refforjnd2xyz.acent', reffornames[2]], 
                       M[reffornames[1], reffornames[2]], 
                       M[reffornames[as.numeric(attr(cdrbackup,'conenumb'))], reffornames[2]])[1, ]
coords[reffornames[2], ] <- fourthpointxyz

# subsequent points
nextpoints <- row.names(M)[!row.names(M) %in% reffornames ]
positions <- lapply(nextpoints, function(x) 
  pos4(M['refforjnd2xyz.acent', reffornames[1]], M['refforjnd2xyz.acent', x],
       M[reffornames[1], x], M[reffornames[as.numeric(attr(cdrbackup,'conenumb'))], x]))
names(positions) <- nextpoints

eucdis <- lapply(positions, function(x) dist(rbind(x, coords[reffornames[2],]))[c(2,3)])
whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - M[reffornames[2], x])))
names(whichdist) <- names(eucdis)

coords[nextpoints, ] <- do.call(rbind,
  lapply(nextpoints, function(x) positions[[x]][whichdist[[x]], ]))
}

if(TRI){
coords <- matrix(NA, nrow=nrow(M), ncol=2, dimnames=list(row.names(M), c('x','y')))

reffornames <- grep('refforjnd2xyz', rownames(coords), value=TRUE)

# first point
coords['refforjnd2xyz.acent', ] <- c(0,0)
# second point
coords['bluref', ] <- c(M['refforjnd2xyz.acent',reffornames[1]],0)
# third point
coords['redref', ] <- pos3(M['refforjnd2xyz.acent',reffornames[1]], M['refforjnd2xyz.acent', reffornames[as.numeric(attr(cdrbackup,'conenumb'))]], M[reffornames[1], reffornames[as.numeric(attr(cdrbackup,'conenumb'))]])[1, ]

# subsequent points
nextpoints <- row.names(M)[!row.names(M) %in% reffornames ]
positions <- lapply(nextpoints, function(x) pos3(M['refforjnd2xyz.acent',reffornames[1]], M['refforjnd2xyz.acent',x], M[reffornames[1],x]))
names(positions) <- nextpoints

eucdis <- lapply(positions, function(x) dist(rbind(x, coords[reffornames[as.numeric(attr(cdrbackup,'conenumb'))],]))[c(2,3)])
whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - M[reffornames[as.numeric(attr(cdrbackup,'conenumb'))], x])))
names(whichdist) <- names(eucdis)

coords[nextpoints, ] <- do.call(rbind,
  lapply(nextpoints, function(x) positions[[x]][whichdist[[x]], ]))
}


chromcoords <- data.frame(coords[nextpoints,])

# Achromatic coords
if('dL' %in% colnames(cdrbackup)){
coldistres <- as.matrix(rbind(cdrbackup[ ,c(1,2,4)], cdrbackup[ ,c(2,1,4)]))

uniquepatches <-  unique(c(coldistres[,1], coldistres[,2]))

M <- matrix(nrow=length(uniquepatches), ncol=length(uniquepatches))

rownames(M) <- uniquepatches
colnames(M) <- uniquepatches

M[coldistres[,1:2] ] <- coldistres[,3]
M[coldistres[,2:1] ] <- coldistres[,3]

class(M) <- 'numeric'
M[is.na(M)] <- 0

# use ind that is the furthest from all others on average as reference
achrocoords <- as.matrix(M[which.max(colMeans(M)),])
chromcoords$lum <- achrocoords[rownames(chromcoords), ]
}

chromcoords
}


fakedata1 <-  sapply(seq(100,500,by = 20), 
                     function(x) rowSums(cbind(dnorm(300:700,x,30), 
                                               dnorm(300:700,x+400,30))))
fakedata2 <- sapply(c(500, 300, 150, 105, 75, 55, 40, 30), 
                     function(x) dnorm(300:700,550,x))

fakedata1 <- as.rspec(data.frame(wl = 300:700, white=rep(1,401), fakedata1))
fakedata1 <- procspec(fakedata1, "max")
fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
fakedata2 <- procspec(fakedata2, "sum")
fakedata2 <- procspec(fakedata2, "min")
fakedata1[,-1] <- fakedata1[,-1]*100
fakedata2[,-1] <- fakedata2[,-1]/max(fakedata2[,-1])*100
fakedata.c <- data.frame(fakedata1, fakedata2[,-1])
fakedata.c <- as.rspec(fakedata.c)

test <- coldist(vismodel(fakedata.c, visual='apis',relative=FALSE), n=c(1,1,2), achro=FALSE)

plot(jnd2xyz(test, TRI=T), pch=20, col=spec2rgb(fakedata.c), cex=2); abline(v=0, lty=3); abline(h=0, lty=3)
#make circles with radius = 1 JND
symbols(tripyke(test)$x,tripyke(test)$y, circles=rep(1, dim(tripyke(test))[1]), add=T, inches=F)
segments(0,0,1,0)


test2 <- coldist(vismodel(fakedata.c, visual='avg.uv',relative=FALSE), n=c(1,2,2,4), achro=FALSE)

scatterplot3d::scatterplot3d(jnd2xyz(test2, TET=T), pch=20, color=spec2rgb(fakedata.c), box=F, angle=70)

# check that Euclidean distances in xyz = jnd
all.equal(as.vector(dist(pyke(test2, TET=T))), test2$dS)
all.equal(as.vector(dist(pyke(test, TRI=T))), test$dS)



