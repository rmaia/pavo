require(pavo)
data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F, scale=10000000)
#attr(aa, 'conenumb') <- 2 # when using pavo release

colspace(aa)

plot(
#all.equal(
coldist(aa,  achro=F, weber.ref='n2')$dS,
coldist2(aa, n=c(1,2), weber.ref=2, achro=F, noise='quantum')$dS
#)
); abline(0,1)

data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F, scale=10000000)
#attr(aa, 'conenumb') <- 3 # when using pavo release

plot(
#all.equal(
coldist(aa, achro=F, weber.ref='n3')$dS,
coldist2(aa, n=c(1,2,2), weber.ref=3, achro=F, noise='quantum')$dS
#)
); abline(0,1)

data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F, scale=10000000)
#attr(aa, 'conenumb') <- 4 # when using pavo release

plot(
#all.equal(
coldist(aa, achro=F)$dS,
coldist2(aa, n=c(1,2,2,4), weber.ref=4, achro=F, noise='quantum')$dS
#)
); abline(0,1)


require(pavo)
packageVersion('pavo') # 0.99
data(sicalis)
aa <- vismodel(sicalis, visual=sensmodel(c(400,500)), rel=F, scale=10000000)
bb <- colspace(aa)
plot(coldist2(aa, n=c(1,2), weber.ref=2, achro=F)$dS ~coldist2(bb, n=c(1,2), weber.ref=2, achro=F)$dS)

aa <- vismodel(sicalis, visual=sensmodel(c(300,400,500)), rel=F, scale=10000000)
bb <- colspace(aa)
plot(coldist2(aa, n=c(1,2,2), weber.ref=3, achro=F)$dS ~coldist2(bb, n=c(1,2,2), weber.ref=3, achro=F)$dS); abline(0,1)


aa <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F, scale=10000000)
bb <- colspace(aa)
plot(coldist2(aa, n=c(1,2,2,4), weber.ref=4, achro=F)$dS ~coldist2(bb, n=c(1,2,2,4), weber.ref=4, achro=F)$dS); abline(0,1)