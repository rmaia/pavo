tripyke <- function(coldistres) {

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


coords <- matrix(0, nrow=nrow(M), ncol=2, dimnames=list(NULL, c('x','y')))

coords[2, ] <- c(M[1,2],0)
# third point
coords[3, ] <- pos3(M[1,2], M[1,3], M[2,3])[1, ]
# subsequent points
positions <- lapply(seq_along(M[,1])[-c(1:3)], function(x) pos3(M[1,2], M[1,x], M[2,x]))
names(positions) <- rownames(M)[-c(1:3)]

eucdis <- lapply(positions, function(x) dist(rbind(x, coords[3,]))[c(2,3)])
whichdist <- lapply(names(eucdis), function(x) which.min(abs(eucdis[[x]] - M[3, x])))

coords[-c(1:3), ] <- do.call(rbind,
  lapply(seq_along(whichdist), function(x) positions[[x]][whichdist[[x]], ]))

row.names(coords) <- row.names(M)  
coords
}


fakedata1 <-  sapply(seq(100,500,by = 20), 
                     function(x) rowSums(cbind(dnorm(300:700,x,30), 
                                               dnorm(300:700,x+400,30))))
fakedata2 <- sapply(c(500, 300, 150, 105, 75, 55, 40, 30), 
                     function(x) dnorm(300:700,550,x))

fakedata1 <- as.rspec(data.frame(wl = 300:700, fakedata1))
fakedata1 <- procspec(fakedata1, "max")
fakedata2 <- as.rspec(data.frame(wl = 300:700, fakedata2))
fakedata2 <- procspec(fakedata2, "sum")
fakedata2 <- procspec(fakedata2, "min")
fakedata1[,-1] <- fakedata1[,-1]*100
fakedata2[,-1] <- fakedata2[,-1]/max(fakedata2[,-1])*100
fakedata.c <- data.frame(fakedata1, fakedata2[,-1])
fakedata.c <- as.rspec(fakedata.c)

test <- coldist(vismodel(fakedata.c, visual='apis',relative=FALSE), n=c(1,1,2), achro=FALSE)

plot(tripyke(test), pch=20, col=spec2rgb(fakedata.c)); abline(h=0, lty=3); abline(v=0, lty=3)
