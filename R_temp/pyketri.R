tripyke <- function(coldistres) {
	
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
	y3 <- sqrt(d13^2 - x3^2)*c(1,-1)
	matrix(c(rep(x3,2),y3), ncol=2, dimnames=list(NULL, c('x','y')))
}


coords <- matrix(NA, nrow=nrow(M), ncol=2, dimnames=list(row.names(M), c('x','y')))

# first point
coords['whiref', ] <- c(0,0)
# second point
coords['redref', ] <- c(M['whiref','redref'],0)
# third point
coords['bluref', ] <- pos3(M['whiref','redref'], M['whiref','bluref'], M['redref','bluref'])[1, ]

# subsequent points
nextpoints <- row.names(M)[!row.names(M) %in% c("whiref","redref", "bluref") ]
positions <- lapply(nextpoints, function(x) pos3(M['whiref','redref'], M['whiref',x], M['redref',x]))
names(positions) <- nextpoints

eucdis <- lapply(positions, function(x) dist(rbind(x, coords['bluref',]))[c(2,3)])
whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - M['bluref', x])))
names(whichdist) <- names(eucdis)

coords[nextpoints, ] <- do.call(rbind,
  lapply(nextpoints, function(x) positions[[x]][whichdist[[x]], ]))

coords[nextpoints,]
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

plot(tripyke(test), pch=20, col=spec2rgb(fakedata.c), cex=2); abline(v=0); abline(h=0)
