require(pavo)
data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F, scale=10000000)
attr(aa, 'conenumb') <- 2 # when using pavo release

#plot(
all.equal(
coldist(aa, vis='di', achro=F)$dS,
coldist2(aa, n=c(1,2), weber.ref=2, achro=F, noise='quantum')$dS
); abline(0,1)

data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F, scale=10000000)
attr(aa, 'conenumb') <- 3 # when using pavo release

#plot(
all.equal(
coldist(aa, vis='tri', achro=F)$dS,
coldist2(aa, n=c(1,2,2), weber.ref=3, achro=F, noise='quantum')$dS
); abline(0,1)


data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F, scale=10000000)
attr(aa, 'conenumb') <- 4 # when using pavo release

#plot(
all.equal(
coldist(aa, vis='tetra', achro=F)$dS,
coldist2(aa, n=c(1,2,2,4,5), weber.ref=4, achro=F, noise='quantum')$dS
); abline(0,1)