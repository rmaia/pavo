require(pavo)
eliz <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_elisabeth', ext='ttt')
eliz <- aggspec(eliz, by=3)
chlo <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_chloropterus', ext='ttt') 
chlo <- aggspec(chlo, 3)
acut <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_acuticaudus', ext='ttt') 
acut <- aggspec(acut, 3)

bodypart<-c('throat','chest','belly','head','neck','mantle','rump','shoulder','wing','tail','cheek')
elizsex <- c('FEM','FEM','FEM','MAL','MAL','MAL','FEM','MAL')
acutsex<-c('MAL','MAL','MAL','FEM','MAL','FEM','FEM','MAL','MAL','FEM','MAL','FEM','FEM','MAL','FEM','MAL','MAL','FEM','FEM','FEM')
chlorsex<- c('MAL','MAL','MAL','MAL','FEM','FEM','MAL','FEM','MAL','FEM','FEM','FEM','MAL','MAL','FEM','FEM','FEM','MAL','FEM','MAL')

names(acut)[-1] <- paste(gsub('.....$','',names(acut))[-1],rep(acutsex,each=11),bodypart, sep='.')
names(eliz)[-1] <- paste(gsub('.....$','',names(eliz))[-1],rep(elizsex,each=11),bodypart, sep='.')
names(chlo)[-1] <- paste(gsub('.....$','',names(chlo))[-1],rep(chlorsex,each=11),bodypart, sep='.')


# elizdat <- data.frame(patch=rep(bodypart, times=length(elizsex)), sex=rep(elizsex, each=11))
# acutdat <- 
# chlordat <- 

# elizabeth + chlor = subspecies

dat <- c(names(eliz), names(chlo)[-1], names(acut)[-1])
specs <- as.rspec(cbind(eliz, chlo[,-1], acut[,-1]))

patch <- sapply(strsplit(names(specs), "\\."), "[", 4)
sex <- sapply(strsplit(names(specs), "\\."), "[", 3)
spp <- sapply(strsplit(names(specs), "\\."), "[", 1)
ids <- sapply(strsplit(names(specs), "\\."), "[", 2)

# get equal sample sizes
# sample from lael, lacl
set.seed(101)
p <- sample(unique(ids[spp=="LAAC"&sex=="MAL"]), size=4)
acut.sub <- cbind(wl=300:700, specs[, ids%in%p])
acut.sub <- as.rspec(acut.sub)

set.seed(101)
p <- sample(unique(ids[spp=="LACL"&sex=="MAL"]), size=4)
chlo.sub <- cbind(wl=300:700, specs[, ids%in%p])
chlo.sub <- as.rspec(chlo.sub)

eliz.sub <- eliz[, grep('MAL', names(eliz))]  # get only males
eliz.sub <- as.rspec(cbind(wl=300:700, eliz.sub))

specs2 <- as.rspec(cbind(acut.sub, chlo.sub[,-1], eliz.sub[,-1]))
patch2 <- sapply(strsplit(names(specs2), "\\."), "[", 4)
sex2 <- sapply(strsplit(names(specs2), "\\."), "[", 3)
spp2 <- sapply(strsplit(names(specs2), "\\."), "[", 1)
ids2 <- sapply(strsplit(names(specs2), "\\."), "[", 2)


# check if males/females different
# lael.sub <- specs[, c(1, grep('LAEL', names(specs)))]
# res.tri <- summary(lael.sub)
# res.tcs <- tcs(vismodel(lael.sub))
# require(circular)
# h.theta <- circular(res$h.theta, modulo='2pi')
# h.phi <- circular(res$h.phi, modulo='pi')
# b2 <- summary(lael.sub)$B2
# sex1 <- sapply(strsplit(names(lael.sub), '\\.'), "[", 3)[-1]
# pat1 <- sapply(strsplit(names(lael.sub), '\\.'), "[", 4)[-1]
# lm1 <- lm(h.theta~sex1+pat1)
# lm2 <- lm(h.phi~sex1+pat1)
# lm3 <- lm(b2~sex1+pat1)
# summary(lm1)
# summary(lm2)
# summary(lm3)
# h.theta marginally significant (p=0.065), brightness and h.phi not
# ok to combine sexes?


# Average spectra by species and patch
specs.m <- aggspec(specs2, by=paste(spp2, patch2)[-1])
head(specs.m)


explorespec(specs.m, by=11)

# 1  2  3
# |  |  |
# |  ----
# |   |
# -----
#   |
#   |
#   |

# 1 = Lamprotornis_acuticaudus (LAAC)
# 2 = Lamprotornis_chloropterus (LACL)
# 3 = Lamprotornis_elisabeth (LAEL)


#  visual modeling
vm1 <- vismodel(specs.m[c(1, grep('LAAC',names(specs.m)))])
vm2 <- vismodel(specs.m[c(1, grep('LACL',names(specs.m)))])
vm3 <- vismodel(specs.m[c(1, grep('LAEL',names(specs.m)))])
vm23 <- apply(abind(vm2$Qi, vm3$Qi, along=3), c(1,2), mean)  # averages each cell

# Tetra colorspace plots
tcs1 <- tcs(vm1)
tcs2 <- tcs(vm2)
tcs3 <- tcs(vm3)

# Convex hulls for patches of species, volume overlap calculations
open3d()
tcsvol(tcs1, col='red')
tcsvol(tcs2, col='green')
tcsvol(tcs3, col='blue')

voloverlap(tcs1, tcs2)
voloverlap(tcs1, tcs3)
voloverlap(tcs2, tcs3)

#### L. elisabeth (blue) is the most 'derived' in color space
#### What patches are causing increased volume?

# Calculate distances between patches of different species

# Visual models
vm1.rel <- vismodel(specs.m[c(1, grep('LAAC',names(specs.m)))], relative=FALSE)
vm2.rel <- vismodel(specs.m[c(1, grep('LACL',names(specs.m)))], relative=FALSE)
vm3.rel <- vismodel(specs.m[c(1, grep('LAEL',names(specs.m)))], relative=FALSE)

# Calculate unstandardized contrasts for LACL-LAEL node
vm23.rel <- apply(abind(vm2.rel$Qi, vm3.rel$Qi, along=3), c(1,2), mean)
substr(rownames(vm23.rel), 1, 4) <- c("ANCE")

# all distances
deltas <- coldist(rbind(vm1.rel$Qi, vm2.rel$Qi, vm3.rel$Qi))

# Distances by patch type
res <- list()
for (i in unique(patch)[-1]) {
sel <- grep(i, deltas$patch1)[grep(i, deltas$patch1)%in%grep(i, deltas$patch2)]
res[[i]] <- deltas[sel, ]
}
res

d12 <- sapply(res, "[[", 3)[1, ]
d13 <- sapply(res, "[[", 3)[2, ]
d23 <- sapply(res, "[[", 3)[3, ]

dists <- cbind(d12, d13, d23)
dists <- data.frame(dists, row.names=NULL)
dists$patch <- factor(names(d12))
head(dists)

# One way to plot the data
require(gridExtra)
g1 <- ggplot(data=dists, aes(x=d23, y=d13)) + geom_point(aes(colour=patch)) + 
             geom_dl(aes(label=patch, colour=patch), method='first.qp') + 
             geom_abline(slope=1) + xlim(-1,8) + ylim(-1,8)
g2 <- ggplot(data=dists, aes(x=d23, y=d12)) + geom_point(aes(colour=patch)) + 
             geom_dl(aes(label=patch, colour=patch), method='first.qp') + 
             geom_abline(slope=1) + xlim(-1,8) + ylim(-1,8)
grid.arrange(g1, g2, nrow=1)

# Another way to look at things
deltas <- coldist(rbind(vm1.rel$Qi, vm23.rel))

# Distances by patch type
res2 <- list()
for (i in unique(patch)[-1]) {
sel <- grep(i, deltas$patch1)[grep(i, deltas$patch1)%in%grep(i, deltas$patch2)]
res2[[i]] <- deltas[sel, ]
}
# res2

deltas <- coldist(rbind(vm2.rel$Qi, vm3.rel$Qi))

res3 <- list()
for (i in unique(patch)[-1]) {
sel <- grep(i, deltas$patch1)[grep(i, deltas$patch1)%in%grep(i, deltas$patch2)]
res3[[i]] <- deltas[sel, ]
}
# res3

# Divergence plot
par(mar=c(5,4,1,1))
barplot(sapply(res3, "[[", 3), ylim=c(-10,10), ylab=expression(paste(Delta,"S (jnds)")), las=2, xlab="", col='black')
barplot(-sapply(res2, "[[", 3), ylim=c(-10,10), las=2, ylab="", axes=F, xaxt='n', add=TRUE, col='black')
abline(h=1, lty=2, col='grey')
abline(h=-1, lty=2, col='grey')
abline(h=0, lwd=1)
arrows(5,-4,5,-7)
arrows(5,4,5,7)
text(5, -8, 'divergence from outgroup')
text(5, 8, 'divergence within node')
mtext(side=1, line=4, "Patch")
