require(RColorBrewer)
require(pavo)
pal <- brewer.pal(7, "Dark2")

# little change #
# Load spectra
eliz <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_elisabeth', 
                ext='ttt')
eliz <- aggspec(eliz, by=3)
chlo <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_chloropterus', 
                ext='ttt')
chlo <- aggspec(chlo, 3)
acut <- getspec('~/Documents/Projects/Starlings - RawData/Starling_irid/Lamprotornis_acuticaudus', 
                ext='ttt') 
acut <- aggspec(acut, 3)


# Define metadata
bodypart<-c('throat','chest','belly','head','neck','mantle','rump','shoulder', 'wing','tail','cheek')
elizsex <- c('FEM','FEM','FEM','MAL','MAL','MAL','FEM','MAL')
acutsex<-c('MAL','MAL','MAL','FEM','MAL','FEM','FEM','MAL','MAL','FEM','MAL', 'FEM','FEM','MAL','FEM','MAL','MAL','FEM','FEM','FEM')
chlorsex<- c('MAL','MAL','MAL','MAL','FEM','FEM','MAL','FEM','MAL','FEM','FEM', 'FEM','MAL','MAL','FEM','FEM','FEM','MAL','FEM','MAL')

# Names of specs
names(acut)[-1] <- paste(gsub('.....$','',names(acut))[-1],rep(acutsex,each=11), 
                         bodypart, sep='.')
names(eliz)[-1] <- paste(gsub('.....$','',names(eliz))[-1],rep(elizsex,each=11),
                         bodypart, sep='.')
names(chlo)[-1] <- paste(gsub('.....$','',names(chlo))[-1],rep(chlorsex,each=11),
                         bodypart, sep='.')@

<<include=FALSE>>=
# Get equal sample sizes by sampling randomly from L. chloropterus, L. acuticaudus
set.seed(101)
p <- sample(unique(ids[spp=="LAAC"&sex=="MAL"]), size = 4)
acut.sub <- cbind(wl = 300:700, specs[, ids%in%p])
acut.sub <- as.rspec(acut.sub)

set.seed(101)
p <- sample(unique(ids[spp=="LACL"&sex=="MAL"]), size=4)
chlo.sub <- cbind(wl = 300:700, specs[, ids%in%p])
chlo.sub <- as.rspec(chlo.sub)

# Get only males from L. elisabeth
eliz.sub <- eliz[, grep('MAL', names(eliz))]  # get only males
eliz.sub <- as.rspec(cbind(wl = 300:700, eliz.sub))
@

<<>>=
# Merge subsetted spectra
specs <- as.rspec(cbind(acut.sub, chlo.sub[,-1], eliz.sub[,-1]))
spp <- sapply(strsplit(names(specs), "\\."), "[", 1)
sex <- sapply(strsplit(names(specs), "\\."), "[", 3)
patch <- sapply(strsplit(names(specs), "\\."), "[", 4)
@

<<include=FALSE>>=
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
specs.sm <- procspec(specs, opt='smooth')
specs.m <- aggspec(specs.sm, by = paste(spp, patch)[-1])
head(specs.m)
explorespec(specs.m, by = 11)


# Run visual models
vm1 <- vismodel(specs.m[c(1, grep('LAAC', names(specs.m)))], visual='star', relative=TRUE)
vm2 <- vismodel(specs.m[c(1, grep('LACL', names(specs.m)))], visual='star', relative=TRUE)
vm3 <- vismodel(specs.m[c(1, grep('LAEL', names(specs.m)))], visual='star', relative=TRUE)



# Calculate tetra colorspace variables
tcs1 <- tcs(vm1)
tcs2 <- tcs(vm2)
tcs3 <- tcs(vm3)


pal <- brewer.pal(11, "Spectral")

# Plot species in colorspace
tcsplot(tcs1, col = pal, size=.01)
tcsvol(tcs1, col='red')
tcspoints(tcs2, col = pal, size=.01)
tcsvol(tcs2, col='blue')
tcspoints(tcs3, col = pal, size=.01)
tcsvol(tcs3, col='green')


# Volume overlap calculations
voloverlap(tcs2, tcs3, plot=T)


# L. elisabeth (blue) is the most spread out in color space. So, what patches 
# are causing the increased volume?

# Calculate chromatic distances between patches of different species
vm1.rel <- vismodel(specs[c(1, grep('LAAC',names(specs)))], visual = 'star', relative = FALSE)
vm2.rel <- vismodel(specs[c(1, grep('LACL',names(specs)))], visual = 'star', relative = FALSE)
vm3.rel <- vismodel(specs[c(1, grep('LAEL',names(specs)))], visual = 'star', relative = FALSE)


# All delta S distances
delta.S <- coldist(rbind(vm1.rel$Qi, vm2.rel$Qi, vm3.rel$Qi), n1 = 1, n2 = 1.38, n3 = 3.34, 
                   n4 = 3.46, v = 0.1)



# Distances by patch type
res <- list()
# patch <- sapply(strsplit(names(specs), "\\."), "[", 4)
for (i in unique(patch)[-1]) {
  sel <- grep(i, delta.S$patch1)[grep(i, delta.S$patch1)%in%grep(i, delta.S$patch2)]
  res[[i]] <- delta.S[sel, ]
}
res[1]  # check output




# Calculate distance from focal species' patch to those of other species
d12 <- matrix(ncol = 2, nrow = 11)
d13 <- matrix(ncol = 2, nrow = 11)
d23 <- matrix(ncol = 2, nrow = 11)

for (i in 1:11) {
  x <- res[[i]][grep('LAAC', res[[i]]$patch1)[grep('LAAC', res[[i]]$patch1) %in% 
                grep('LACL', res[[i]]$patch2)], ]
  d12[i, 1] <- mean(x$tetra.dS)
  d12[i, 2] <- sd(x$tetra.dS)
}
rownames(d12) <- names(res)
colnames(d12) <- c('mean', 'sd')

for (i in 1:11) {
  x <- res[[i]][grep('LAAC', res[[i]]$patch1)[grep('LAAC', res[[i]]$patch1) %in% 
                grep('LAEL', res[[i]]$patch2)], ]
  d13[i, 1] <- mean(x$tetra.dS)
  d13[i, 2] <- sd(x$tetra.dS)
}
rownames(d13) <- names(res)
colnames(d13) <- c('mean', 'sd')

for (i in 1:11) {
  x <- res[[i]][grep('LACL', res[[i]]$patch1)[grep('LACL', res[[i]]$patch1) %in% 
                grep('LAEL', res[[i]]$patch2)], ]
  d23[i, 1] <- mean(x$tetra.dS)
  d23[i, 2] <- sd(x$tetra.dS)
}
rownames(d23) <- names(res)
colnames(d23) <- c('mean', 'sd')


# Plot mean +/- SD of color distances by patch
par(mar=c(4,4,2,2), oma=c(2,0,0,0))
pad <- .15
plot(d23[,1]~I(c(1:11)), ylim = c(-16, 16), xlab = "", 
     ylab = expression(paste(Delta, "S (jnds)")), xaxt = 'n', col = pal[1], 
     pch = 16, xlim = c(0.5, 11.5))
segments(1:11, d23[, 1] - d23[, 2], 1:11, d23[, 1] + d23[, 2], col = pal[1])
points(-d13[, 1]~I(c(1:11) - pad), pch = 16, col = pal[2])
segments(1:11 - pad, -d13[, 1] - d13[, 2], 1:11 - pad, -d13[, 1] + d13[, 2], 
         col = pal[2])
points(-d12[, 1]~I(c(1:11) + pad), pch = 16, col = pal[3])
segments(1:11 + pad, -d12[, 1] - d12[, 2], 1:11 + pad, -d12[, 1] + d12[, 2], 
         col = pal[3])
abline(h=0)
abline(h = c(1,-1), lty = 3)
axis(side = 1, at = c(1:11), labels = names(res), las = 2)
legend('topleft', bty='n', lty = 1, pch = 16, col = pal[1:3], 
       legend = c("chloropterus-elisabeth", "acuticaudus-elisabeth", "acuticaudus-chloropterus"))
mtext(side = 1, "Plumage patch", line=4)




# Another way to look at things: calculate unstandardized contrasts (mean usml) for 
# LACL-LAEL node and compare distance to outgroup (L. acutcau)

# <<>>=
# vm23.rel <- apply(abind(vm2.rel$Qi, vm3.rel$Qi, along = 3), c(1, 2), mean)
# substr(rownames(vm23.rel), 1, 4) <- c("ANCE")

# delta.S2 <- coldist(rbind(vm1.rel$Qi, vm23.rel))

# res2 <- list()
# for (i in unique(patch)[-1]) {
# sel <- grep(i, delta.S2$patch1)[grep(i, delta.S2$patch1)%in%grep(i, delta.S2$patch2)]
# res2[[i]] <- delta.S2[sel, ]
# }
# res2[1]  # check output

# d23b <- matrix(ncol=2, nrow=11)
# for (i in 1:11) {
#   x <- res2[[i]][grep('LAAC', res2[[i]]$patch1)[grep('LAAC', res2[[i]]$patch1) %in% 
#                  grep('ANCE', res2[[i]]$patch2)], ]
#   d23b[i, 1] <- mean(x$tetra.dS)
#   d23b[i, 2] <- sd(x$tetra.dS)
# }
# rownames(d23b) <- names(res2)
# colnames(d23b) <- c('mean', 'sd')

# # Distances by patch type using unstandardized contrasts
# plot(d23[, 1]~I(c(1:11)), ylim = c(-14, 14), xlab = "", ylab = 'deltaS', 
#      xaxt = 'n', col = pal[1], pch = 16, xlim = c(0.5,11.5))
# segments(1:11, d23[,1]-d23[,2], 1:11, d23[,1]+d23[,2], col=pal[1])
# points(-d23b[, 1]~c(1:11), pch = 16, col = pal[2])
# segments(1:11, -d23b[, 1] - d23b[, 2], 1:11, -d23b[, 1] + d23b[, 2], col = pal[2])
# abline(h = 0)
# abline(h = c(1, -1), lty = 3)
# axis(side = 1, at = c(1:11), labels = names(res), las = 2)
# legend('topleft', lty = 1, pch = 16, col = pal[1:2], 
#        legend = c('chloropterus-elisabeth', 'acuticaudus-ancestor'), bty = 'n')
