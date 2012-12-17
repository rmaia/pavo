# benchmark pavo overlap
require(pavo)
load('~/github/pavo/toydata/ACE.RData')
source('~/Desktop/voverlap_old.R')

# Create volumes
# box with sides = 1x1x1:
v1 <- cbind(x=c(0,1,1,1,0,0,0,1), y=c(0,0,1,1,1,0,1,0), z=c(0,0,0,1,1,1,0,1))
# box inside box 1, sides = .5x.5x.5:
v2 <- cbind(x=c(0,.5,.5,.5,0,0,0,.5), y=c(0,0,.5,.5,.5,0,.5,0), z=c(0,0,0,.5,.5,.5,0,.5))
# box 2 moved over 0.4 units:
v3 <- v2
v3[,1] <- v3[,1] - 0.4

# Calculate actual overlaps between boxes 1-2 and 1-3
v12.real <- .5^3  # since box 2 is totally inside box 1
v13.real <- (.1*.5*.5)/(1+(.5^3 - .1*.5*.5))

# Visualize results
system.time(voloverlap(v1, v2))  # 0.023 seconds
view3d(theta=35, phi=25)
v12.m1 <- voloverlap(v1, v2, plot=T)$vboth
v13.m1 <- voloverlap(v1, v3, plot=T)$vboth

# Calculate volumes over many nsamps using Monte Carlo method
# 10^k, where k=2,3,4,5,6
# Time required:
# 1e2 = .17 s x 10 = 1.7 s
# 1e3 = 1.7 s x 10 = 17 s
# 1e4 = 17 s x 10 = 2.8 min
# 1e5 = 170s x 10 = 28 min
# 1e6 = 1700s x 10 = 280 min

# Run simulations
sim <- 10^c(2:5)
res1 <- list()
length(res1) <- length(sim)
for (i in seq_along(sim)){
  for (j in 1:1:10){
    res1[[i]][j] <- voloverlap.old(v1, v3, nsamp=sim[i])$pboth
  }
}
# saveRDS(res1, 'mc_overlap_test.rda')

res1 <- readRDS('ms_overlap_test.rda')

# Plot results from MC analysis
means <- unlist(lapply(res1, mean))
ci.l <- sapply(1:4, function(i)quantile(res1[[i]], probs=.025))
ci.u <- sapply(1:4, function(i)quantile(res1[[i]], probs=.975))

par(mar=c(4,4,2,2))
plot(means~sim, ylim=range(ci.l,ci.u), xlab="samples", ylab="estimated overlap",
     log='x', type='l', xaxt='n', lty=1, xaxs='i', las=1)
polygon(x=c(sim,rev(sim)), y=c(ci.l, rev(ci.u)), border=NA, col=rgb(0,0,0, 
        alpha=.15))
axis(1, at=c(1e2,1e3,1e4,1e5))
abline(h = v13.real, lty=2)
