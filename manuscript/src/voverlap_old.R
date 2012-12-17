voloverlap.old <- function(tcsres1,tcsres2, nsamp=1000, plot=FALSE, size=0.001){

dat1 <- tcsres1[, c('x', 'y', 'z')] 
dat2 <- tcsres2[, c('x', 'y', 'z')] 

# calculate their volumes

vol1 <- convhulln(dat1, 'FA')$vol
vol2 <- convhulln(dat2, 'FA')$vol

# sample random points
pmin <- apply(rbind(dat1,dat2),2,min)
pmax <- apply(rbind(dat1,dat2),2,max)

samples <- apply(rbind(pmin,pmax), 2, function(x) runif(nsamp,x[1],x[2]))

sindex <- 1:dim(samples)[1]

newvol1 <- sapply(sindex, function(x) convhulln(rbind(dat1,samples[x,]),'FA')$vol)
newvol2 <- sapply(sindex, function(x) convhulln(rbind(dat2,samples[x,]),'FA')$vol)

# points that are within each volume

invol1 <- sapply(newvol1, function(x) isTRUE(x<=vol1))
invol2 <- sapply(newvol2, function(x) isTRUE(x<=vol2))

# how many points are in each category

s_in1 <- length(which(invol1))
s_in2 <- length(which(invol2))

s_inboth <- length(which(invol1 & invol2))

s_ineither <- length(which(invol1 | invol2))

# points in both relative points in smallest

psmallest <- s_inboth / c(s_in1,s_in2)[which.min(c(vol1,vol2))]

# points in both relative to total points in both

pboth <- s_inboth / s_ineither

if(plot==T){
  open3d(FOV=1, mouseMode=c('zAxis','xAxis','zoom'))
  ttvol(dat1, col='red', fill=F)
  ttvol(dat2, col='blue', fill=F)

  spheres3d(samples[which(invol1 & !invol2),], type='s', lit=F, radius=size, col='red')
  spheres3d(samples[which(invol2 & !invol1),], type='s', lit=F, radius=size, col='blue')  

  if(s_inboth > 0){  
    spheres3d(samples[which(invol1 & invol2),], type='s', lit=F, radius=size, col='black')
    }
  }

data.frame(vol1, vol2, s_in1,s_in2,s_inboth,s_ineither,psmallest,pboth)

}