# benchmark pavo overlap
library(pavo)
load('~/github/pavo/toydata/ACE.RData')
source('/Users/chad/github/pavo/manuscript/src/voverlap_old.R')

#-------------------------------------------------------------------------------
# Create volumes
#-------------------------------------------------------------------------------

# Cube
c1 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1),
            y = c(0, 0, 1, 1, 1, 0, 1, 0),
            z = c(0, 0, 0, 1, 1, 1, 0, 1))  # sides = 1
c2 <- cbind(x = c(0, .5, .5, .5, 0, 0, 0, .5),
            y = c(0, 0, .5, .5, .5, 0, .5, 0),
            z = c(0, 0, 0, .5, .5, .5, 0, .5))  # inside box 1, sides = .5
c3 <- c2
c3[,1] <- c3[,1] - 0.4  # box 2 moved over 0.4 units

# Tetrahedron
th1 <- cbind(x = c(0, 1, .5, .5),
             y = c(0, 0, sqrt(3)/2, sqrt(3)/4),
             z = c(0, 0, 0, sqrt(3)/2))
th2 <- th1/2
th3 <- th1
th3[,3] <- th3[,3] + sqrt(3)/4  # shift up z-axis by 1/2 of height

# Cuboid
cb1 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1),
             y = c(0, 0, 1/2, 1/2, 1/2, 0, 1/2, 0),
             z = c(0, 0, 0, 1/2, 1/2, 1/2, 0, 1/2))  # sides = 1

cb2 <- cbind(x = c(0, 1, 1, 1, 0, 0, 0, 1) - 0.4,
             y = c(0, 0, 1/2, 1/2, 1/2, 0, 1/2, 0),
             z = c(0, 0, 0, 1/2, 1/2, 1/2, 0, 1/2))  # sides = 1

cb3 <- cb1 * 0.5
cb3[,1] <- cb3[,1] - 0.4

#-------------------------------------------------------------------------------
# Calculate real + pavo overlap
#-------------------------------------------------------------------------------
# Cube
# vo_c12_real <- .5^3  # since box 2 is totally inside box 1
vo_c13_real <- (.1*.5*.5)/(1+(.5^3 - .1*.5*.5))
# system.time(voloverlap(v1, v2))  # 0.023 seconds
# vo_c12_pavo <- voloverlap(v1, v2, plot=T)$vboth
vo_c13_pavo <- voloverlap(c1, c3, plot=T)$vboth
vo_c13_real/vo_c13_pavo

# Tetrahedron
A_0 = .5*(sqrt(3)/2)  # base area
h = sqrt(3)/2  # height
v_th1 <- (1/3)*A_0*h
v_th3 <- (1/3)*(.25*sqrt(3)/4)*sqrt(3)/4
vo_th_real <- v_th3/(v_th1*2-v_th3)
vo_th_pavo <- voloverlap(th1, th3, plot=T)$vboth
vo_th_real/vo_th_pavo  # equal to 1 (good)
system.time(voloverlap(th1, th3))  # time benchmark, t=0.029 seconds

# Cuboid
v_cb1 <- 1*.5*.5
v_cb2 <- v_cb3 <- .5*.25*.25
vo_cb13 <- (.1*.25*.25)
vo_cb_real <- vo_cb13/(v_cb1+v_cb3-vo_cb13)
vo_cb_pavo <- voloverlap(cb1, cb3, plot=T)$vboth
vo_cb_real/vo_cb_pavo

#-------------------------------------------------------------------------------
# Run monte carlo simulations (takes ~30 minutes for each loop)
#-------------------------------------------------------------------------------

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
  for (j in 1:1:10){
    res1[[i]][j] <- voloverlap.old(c1, c3, nsamp=sim[i])$pboth
  }
}
# saveRDS(res1, 'mc_overlap_cube.rda')

# Tetrahedron
sim <- 10^c(2:5)
res2 <- list()
length(res2) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:1:10){
    res2[[i]][j] <- voloverlap.old(th1, th3, nsamp=sim[i])$pboth
  }
}
# saveRDS(res2, 'mc_overlap_tetrahedron.rda')

# Cuboid
sim <- 10^c(2:5)
res3 <- list()
length(res3) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:1:10){
    res3[[i]][j] <- voloverlap.old(cb1, cb3, nsamp=sim[i])$pboth
  }
}
saveRDS(res3, 'mc_overlap_cuboid.rda')

#-------------------------------------------------------------------------------
# Calculate MC stats
#-------------------------------------------------------------------------------
res1 <- readRDS('mc_overlap_cube.rda')
res2 <- readRDS('mc_overlap_tetrahedron.rda')
res3 <- readRDS('mc_overlap_cuboid.rda')

means <- unlist(lapply(res1, mean))
ci.l <- sapply(1:4, function(i)quantile(res1[[i]], probs=.025))
ci.u <- sapply(1:4, function(i)quantile(res1[[i]], probs=.975))

means2 <- unlist(lapply(res2, mean))
ci.l3 <- sapply(1:4, function(i)quantile(res2[[i]], probs=.025))
ci.u2 <- sapply(1:4, function(i)quantile(res2[[i]], probs=.975))

means3 <- unlist(lapply(res3, mean))
ci.l3 <- sapply(1:4, function(i)quantile(res3[[i]], probs=.025))
ci.u3 <- sapply(1:4, function(i)quantile(res3[[i]], probs=.975))

#-------------------------------------------------------------------------------
# Plot results from MC analysis
#-------------------------------------------------------------------------------
pdf(width=9, height=3.13, file="mc_results.pdf")
par(mar=c(1,2,1,2), oma=c(4,4,0,0), mfrow=c(1, 3))
# Cube
plot(means~sim, ylim=range(ci.l,ci.u), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l, rev(ci.u)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(1, at=c(1e2,1e3,1e4,1e5))
abline(h = vo_c13_real, lty=1)
# Tetrahedron
plot(means2~sim, ylim=range(ci.l2,ci.u2), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l2, rev(ci.u2)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(1, at=c(1e2,1e3,1e4,1e5))
abline(h = vo_th_real, lty=1)
# Cuboid
plot(means3~sim, ylim=range(ci.l3,ci.u3), log='x', type='l', xaxt='n', lty=2, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l3, rev(ci.u3)), border=NA, col=rgb(0,0,0, alpha=.15))
axis(side=1, at=c(1e2,1e3,1e4,1e5))
abline(h = vo_cb_real, lty=1)
mtext(side=1, outer=T, line=2.5, "samples")
mtext(side=2, outer=T, line=2.5, "estimated overlap")
dev.off()

# Visualize results
voloverlap(c1, c3, plot=T)$vboth
view3d(theta=-45, phi=30)
rgl.snapshot("cube.png")
voloverlap(th1, th3, plot=T)$vboth
view3d(theta=-80, phi=50)
rgl.snapshot("tetrahedron.png")
voloverlap(cb1, cb3, plot=T)$vboth
view3d(theta=-45, phi=30)
rgl.snapshot("cuboid.png")
