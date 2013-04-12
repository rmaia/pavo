#----------------------------------------------------------------------------
# BENCHMARK PAVO OVERLAP ANALYSIS
#----------------------------------------------------------------------------

# Setup
library(pavo)
library(MASS)
library(plyr)
load('~/github/pavo/toydata/ACE.RData')
source('/Users/chad/github/pavo/manuscript/src/voverlap_old.R')

#----------------------------------------------------------------------------
# Shape analysis (verify pavo's methodology)
#----------------------------------------------------------------------------

# 1. Cube
c1 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1), y = c(0, 0, 1, 1, 1, 0, 1, 0), z = c(0, 0, 0, 1, 1, 1, 0, 1))  # sides = 1
c2 <- cbind(x = c(0, .5, .5, .5, 0, 0, 0, .5), y = c(0, 0, .5, .5, .5, 0, .5, 0), z = c(0, 0, 0, .5, .5, .5, 0, .5))  # inside box 1, sides = .5
c3 <- c2
c3[,1] <- c3[,1] - 0.4  # box 2 moved over 0.4 units
# Calculate actual, pavo overlap
pvo_c13_real <- (.1*.5*.5)/(1+(.5^3 - .1*.5*.5))
pvo_c13_pavo <- voloverlap(c1, c3, plot=T)$vboth
pvo_c13_real/pvo_c13_pavo


# 2. Tetrahedron
th1 <- cbind(x = c(0, 1, .5, .5), y = c(0, 0, sqrt(3)/2, sqrt(3)/4), z = c(0, 0, 0, sqrt(3)/2))
th1[,1] <- th1[,1] - 0.5
th1[,2] <- th1[,2] - sqrt(3)/4
th2 <- th1/2
th2[,3] <- th2[,3] + sqrt(3)/4 + sqrt(3)/8
# new height = sqrt(3)/4
# shift up sqrt(3)/8
# height of overlap piece = sqrt(3)/8, overlap side = .25
# thus, overlap volume = (1/3)*(.25^2)*sqrt(3)/4*(sqrt(3)/8)
# th3 <- th1
# th3[,3] <- th3[,3] + sqrt(3)/4  # shift up z-axis by 1/2 of height
voloverlap(th1, th2, plot=T)

# 3. Cuboid
cb1 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1), y = c(0, 0, 1/2, 1/2, 1/2, 0, 1/2, 0), z = c(0, 0, 0, 1/2, 1/2, 1/2, 0, 1/2))  # sides = 1
cb2 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1) - 0.4, y = c(0, 0, 1/2, 1/2, 1/2, 0, 1/2, 0), z = c(0, 0, 0, 1/2, 1/2, 1/2, 0, 1/2))  # sides = 1
cb3 <- cb1 * 0.5
cb3[,1] <- cb3[,1] - 0.4

# Calculate real + pavo overlap

# Tetrahedron
A_0 = sqrt(3)/4  # base area
h = sqrt(3)/2  # height
v_th1 <- (1/3)*A_0*h
v_th2 <- (1/3)*(.5^2*sqrt(3)/4*sqrt(3)/4)
vo_th <- (1/3)*(.25^2)*sqrt(3)/4*(sqrt(3)/8)  # overlapping volume
pvo_th_real <- vo_th/(v_th1+v_th2-vo_th)
pvo_th_pavo <- voloverlap(th1, th2, plot=T)$vboth
pvo_th_real/pvo_th_pavo  # equal to 1 (good)
system.time(voloverlap(th1, th2))  # time benchmark, t=0.029 seconds

# Cuboid
v_cb1 <- 1*.5*.5
v_cb2 <- v_cb3 <- .5*.25*.25
vo_cb13 <- (.1*.25*.25)
pvo_cb_real <- vo_cb13/(v_cb1+v_cb3-vo_cb13)
pvo_cb_pavo <- voloverlap(cb1, cb3)$vboth
pvo_cb_real/pvo_cb_pavo

# Run monte carlo simulations (takes ~30 minutes for each loop)

# Calculate volumes over many nsamps using Monte Carlo method
# 10^k, where k=2,3,4,5,6
# Time required:
# 1e2 = .17 s x 10 = 1.7 s
# 1e3 = 1.7 s x 10 = 17 s
# 1e4 = 17 s x 10 = 2.8 min
# 1e5 = 170s x 10 = 28 min
# 1e6 = 1700s x 10 = 280 min

# Cube
sim <- 10^c(2:5)
res1 <- list()
length(res1) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:10){
    res1[[i]][j] <- voloverlap.old(c1, c3, nsamp=sim[i])$pboth
  }
}
# saveRDS(res1, 'mc_overlap_cube.rda')

# Tetrahedron
sim <- 10^c(2:5)
res2 <- list()
length(res2) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:10){
    res2[[i]][j] <- voloverlap.old(th1, th2, nsamp=sim[i])$pboth
  }
}
saveRDS(res2, 'mc_overlap_tetrahedron.rda')

# Cuboid
sim <- 10^c(2:5)
res3 <- list()
length(res3) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:10){
    res3[[i]][j] <- voloverlap.old(cb1, cb3, nsamp=sim[i])$pboth
  }
}
saveRDS(res3, 'mc_overlap_cuboid.rda')


# Calculate MC stats
setwd("/Users/chad/github/pavo/manuscript/fig")
res1 <- readRDS('mc_overlap_cube.rda')
res2 <- readRDS('mc_overlap_tetrahedron.rda')
res3 <- readRDS('mc_overlap_cuboid.rda')
sim <- 10^(2:5)
means <- unlist(lapply(res1, mean))
ci.l <- sapply(1:4, function(i)quantile(res1[[i]], probs=.025))
ci.u <- sapply(1:4, function(i)quantile(res1[[i]], probs=.975))
means2 <- unlist(lapply(res2, mean))
ci.l2 <- sapply(1:4, function(i)quantile(res2[[i]], probs=.025))
ci.u2 <- sapply(1:4, function(i)quantile(res2[[i]], probs=.975))
means3 <- unlist(lapply(res3, mean))
ci.l3 <- sapply(1:4, function(i)quantile(res3[[i]], probs=.025))
ci.u3 <- sapply(1:4, function(i)quantile(res3[[i]], probs=.975))

# Plot results from MC analysis
pdf(width=9, height=3.13, file="si-mc_overlap_shapes.pdf")
par(mar=c(1,2,1,2), oma=c(4,4,0,0), mfrow=c(1, 3))
# Cube
plot(means~sim, ylim=range(ci.l,ci.u), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l, rev(ci.u)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(1, at=c(1e2,1e3,1e4,1e5))
abline(h = pvo_c13_pavo, lty=1)
# Tetrahedron
plot(means2~sim, ylim=range(ci.l2,.03), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l2, rev(ci.u2)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(1, at=c(1e2,1e3,1e4,1e5))
abline(h = pvo_th_pavo, lty=1)
# Cuboid
plot(means3~sim, ylim=range(ci.l3,ci.u3), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l3, rev(ci.u3)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(side=1, at=c(1e2,1e3,1e4,1e5))
abline(h = pvo_cb_real, lty=1)
mtext(side=1, outer=T, line=2.5, "samples")
mtext(side=2, outer=T, line=2.5, "estimated overlap")
dev.off()

# Visualize results
voloverlap(c1, c3, plot=T)$vboth
view3d(theta=-45, phi=30)
rgl.snapshot("cube.png")
voloverlap(th1, th2, plot=T)$vboth
view3d(theta=70, phi=40)
rgl.snapshot("tetrahedron.png")
voloverlap(cb1, cb3, plot=T)$vboth
view3d(theta=-45, phi=30)
rgl.snapshot("cuboid.png")

#------------------------------------------------------------------------------
# Sphere-like analysis to benchmark MC method
#------------------------------------------------------------------------------

# Create random data
set.seed(101)
sph1 <- mvrnorm(n=100, mu=c(0,0,0), Sigma=diag(c(1,1,1)))  # random data
colnames(sph1) <- c('x', 'y', 'z')
# Duplicate and shift clouds x3
sph2 <- lapply(1:5, function(x) cbind(sph1[, 1:2], z = sph1[, 3] + seq(1.2, 5, length=5)[x]))  # cloud of points shifted along z axis

# Calculate pavo overlap
vo_sph_pavo <- sapply(1:3, function(x) voloverlap(sph1, sph2[[x]])$vboth)
vo_sph_pavo

# Run MC simulations
nsamp <- 10^(2:5)
# big overlap
sims_mc_sph2.1 <- list()
for (i in 1:length(nsamp))
  sims_mc_sph2.1[[i]] <- sapply(1:10, function(x) voloverlap.old(sph1, sph2[[1]], nsamp=nsamp[i])$pboth)
# medium overlap
sims_mc_sph2.2 <- list()
for (i in 1:length(nsamp))
  sims_mc_sph2.2[[i]] <- sapply(1:10, function(x) voloverlap.old(sph1, sph2[[2]], nsamp=nsamp[i])$pboth)
# small overlap
sims_mc_sph2.3 <- list()
for (i in 1:length(nsamp))
  sims_mc_sph2.3[[i]] <- sapply(1:10, function(x) voloverlap.old(sph1, sph2[[3]], nsamp=nsamp[i])$pboth)

# Combine results and expore
out <- list(sims_mc_sph2.1, sims_mc_sph2.2, sims_mc_sph2.3)
saveRDS(out, 'mc_overlap_sphere_redo.rda')

# Load MC results and calculate stats
out <- readRDS("mc_overlap_sphere_redo.rda")
m1 <- sapply(out[[1]], mean)
m2 <- sapply(out[[2]], mean)
m3 <- sapply(out[[3]], mean)
cl1 <- sapply(out[[1]], quantile, probs=.025)
cl2 <- sapply(out[[2]], quantile, probs=.025)
cl3 <- sapply(out[[3]], quantile, probs=.025)
cu1 <- sapply(out[[1]], quantile, probs=.975)
cu2 <- sapply(out[[2]], quantile, probs=.975)
cu3 <- sapply(out[[3]], quantile, probs=.975)

# Plot results
pdf(width=8, height=3.3, "si-mc_overlap_cloud.pdf")
par(mar=c(2,2,2,2), mfrow=c(1,3), oma=c(3,3,0,0))
plot(m1~nsamp, ylim=range(cl1, cu1), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1, xlab="samples", ylab="estimated overlap")
axis(side=1, at=c(1e2,1e3,1e4,1e5))
polygon(x=c(nsamp, rev(nsamp)), y=c(cl1, rev(cu1)), border=NA, col=rgb(0,0,0, alpha=.15))
abline(h = vo_sph_pavo[1], lty=1)
plot(m2~nsamp, ylim=range(cl2, cu2), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1, xlab="samples", ylab="estimated overlap")
axis(side=1, at=c(1e2,1e3,1e4,1e5))
polygon(x=c(nsamp, rev(nsamp)), y=c(cl2, rev(cu2)), border=NA, col=rgb(0,0,0, alpha=.15))
abline(h = vo_sph_pavo[2], lty=1)
plot(m3~nsamp, ylim=range(cl3, cu3), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1, xlab="samples", ylab="estimated overlap")
axis(side=1, at=c(1e2,1e3,1e4,1e5))
polygon(x=c(nsamp, rev(nsamp)), y=c(cl3, rev(cu3)), border=NA, col=rgb(0,0,0, alpha=.15))
abline(h = vo_sph_pavo[3], lty=1)
mtext(side=1, "samples", line=1.5, outer=T)
mtext(side=2, "estimated overlap", line=1.5, outer=T)
dev.off()

# Visualize shapes
voloverlap(sph1, sph2[[1]], plot=T)$vboth
view3d(theta=0, phi=90)
rgl.snapshot("spherelike-1.png")
voloverlap(sph1, sph2[[2]], plot=T)$vboth
view3d(theta=0, phi=90)
rgl.snapshot("spherelike-2.png")
voloverlap(sph1, sph2[[3]], plot=T)$vboth
view3d(theta=0, phi=90)
rgl.snapshot("spherelike-3.png")
