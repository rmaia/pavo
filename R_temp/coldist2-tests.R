################
# PAVO V 0.5-6 #
################

install.packages('pavo')
require(pavo)
data(sicalis)


# DICHROMATIC

old.di.scale <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F, scale=10000000)
old.di <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F)

cd.old.di <- coldist(old.di, n1=1, n2=2, n4=2, v=0.1*sqrt(2), vis='di')
cd.old.di.s <- coldist(old.di.scale, n1=1, n2=2, n4=2, v=0.1*sqrt(2), vis='di')
cd.old.di.sq <- coldist(old.di.scale, n1=1, n2=2, n4=2, v=0.1*sqrt(2), noise='quantum', vis='di')

plot(cd.old.di$dS~cd.old.di.s$dS); abline(0,1)
plot(cd.old.di$dS~cd.old.di.sq$dS); abline(0,1)
plot(cd.old.di.s$dS~cd.old.di.sq$dS); abline(0,1)

plot(cd.old.di$dL~cd.old.di.s$dL); abline(0,1)
plot(cd.old.di$dL~cd.old.di.sq$dL); abline(0,1)
plot(cd.old.di.s$dL~cd.old.di.sq$dL); abline(0,1)


# TRICHROMATIC

old.tri.scale <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F, scale=10000000)
old.tri <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F)

cd.old.tri <- coldist(old.tri, n1=1, n2=2, n4=2, v=0.1*sqrt(2), vis='tri')
cd.old.tri.s <- coldist(old.tri.scale, n1=1, n2=2, n4=2, v=0.1*sqrt(2), vis='tri')
cd.old.tri.sq <- coldist(old.tri.scale, n1=1, n2=2, n4=2, v=0.1*sqrt(2), noise='quantum', vis='tri')

plot(cd.old.tri$dS~cd.old.tri.s$dS); abline(0,1)
plot(cd.old.tri$dS~cd.old.tri.sq$dS); abline(0,1)
plot(cd.old.tri.s$dS~cd.old.tri.sq$dS); abline(0,1)

plot(cd.old.tri$dL~cd.old.tri.s$dL); abline(0,1)
plot(cd.old.tri$dL~cd.old.tri.sq$dL); abline(0,1)
plot(cd.old.tri.s$dL~cd.old.tri.sq$dL); abline(0,1)


# TETRACHROMATIC

old.tet.scale <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F, scale=10000000)
old.tet <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F)

cd.old.tet <- coldist(old.tet, n1=1, n2=2, n4=4, v=0.1*sqrt(4), vis='tetra')
cd.old.tet.s <- coldist(old.tet.scale, n1=1, n2=4, n4=4, v=0.1*sqrt(4), vis='tetra')
cd.old.tet.sq <- coldist(old.tet.scale, n1=1, n2=4, n4=4, v=0.1*sqrt(4), noise='quantum', vis='tetra')

plot(cd.old.tet$dS~cd.old.tet.s$dS); abline(0,1)
plot(cd.old.tet$dS~cd.old.tet.sq$dS); abline(0,1)
plot(cd.old.tet.s$dS~cd.old.tet.sq$dS); abline(0,1)

plot(cd.old.tet$dL~cd.old.tet.s$dL); abline(0,1)
plot(cd.old.tet$dL~cd.old.tet.sq$dL); abline(0,1)
plot(cd.old.tet.s$dL~cd.old.tet.sq$dL); abline(0,1)

save(list=ls(), file='github/pavo/R_temp/oldcoldist.RData')

################
# PAVO V 1.0.0 #
################

devtools::install_git('github/pavo')
require(pavo)
data(sicalis)

# DICHROMATIC

new.di.scale <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F, scale=10000000)
new.di <- vismodel(sicalis, visual=sensmodel(c(400,600)), rel=F)

cd.new.di <- coldist(new.di, n=c(1,2), weber=0.1, weber.achro=0.1)
cd.new.di.s <- coldist(new.di.scale, n=c(1,2), weber=0.1, weber.achro=0.1)
cd.new.di.sq <- coldist(new.di.scale, n=c(1,2), weber=0.1, weber.achro=0.1, noise='quantum')

plot(cd.new.di$dS~cd.new.di.s$dS); abline(0,1)
plot(cd.new.di$dS~cd.new.di.sq$dS); abline(0,1)
plot(cd.new.di.s$dS~cd.new.di.sq$dS); abline(0,1)

plot(cd.new.di$dL~cd.new.di.s$dL); abline(0,1)
plot(cd.new.di$dL~cd.new.di.sq$dL); abline(0,1)
plot(cd.new.di.s$dL~cd.new.di.sq$dL); abline(0,1)


# TRICHROMATIC

new.tri.scale <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F, scale=10000000)
new.tri <- vismodel(sicalis, visual=sensmodel(c(400,500,600)), rel=F)

cd.new.tri <- coldist(new.tri, n=c(1,2,2), weber=0.1, weber.achro=0.1)
cd.new.tri.s <- coldist(new.tri.scale, n=c(1,2,2), weber=0.1, weber.achro=0.1)
cd.new.tri.sq <- coldist(new.tri.scale, n=c(1,2,2), weber=0.1, weber.achro=0.1, noise='quantum')

plot(cd.new.tri$dS~cd.new.tri.s$dS); abline(0,1)
plot(cd.new.tri$dS~cd.new.tri.sq$dS); abline(0,1)
plot(cd.new.tri.s$dS~cd.new.tri.sq$dS); abline(0,1)

plot(cd.new.tri$dL~cd.new.tri.s$dL); abline(0,1)
plot(cd.new.tri$dL~cd.new.tri.sq$dL); abline(0,1)
plot(cd.new.tri.s$dL~cd.new.tri.sq$dL); abline(0,1)


# TETRACHROMATIC

new.tet.scale <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F, scale=10000000)
new.tet <- vismodel(sicalis, visual=sensmodel(c(300,400,500,600)), rel=F)

cd.new.tet <- coldist(new.tet, n=c(1,2,2,4), weber=0.1, weber.achro=0.1)
cd.new.tet.s <- coldist(new.tet.scale, n=c(1,2,2,4), weber=0.1, weber.achro=0.1)
cd.new.tet.sq <- coldist(new.tet.scale, n=c(1,2,2,4), weber=0.1, weber.achro=0.1, noise='quantum')

plot(cd.new.tet$dS~cd.new.tet.s$dS); abline(0,1)
plot(cd.new.tet$dS~cd.new.tet.sq$dS); abline(0,1)
plot(cd.new.tet.s$dS~cd.new.tet.sq$dS); abline(0,1)

plot(cd.new.tet$dL~cd.new.tet.s$dL); abline(0,1)
plot(cd.new.tet$dL~cd.new.tet.sq$dL); abline(0,1)
plot(cd.new.tet.s$dL~cd.new.tet.sq$dL); abline(0,1)


save(list=ls(), file='github/pavo/R_temp/newcoldist.RData')


#######################
# COMPARING OLD V NEW #
#######################

load('github/pavo/R_temp/oldcoldist.RData')
load('github/pavo/R_temp/newcoldist.RData')

plot(cd.old.di$dS ~ cd.new.di$dS); abline(0,1)
plot(cd.old.di.s$dS ~ cd.new.di.s$dS); abline(0,1)
plot(cd.old.di.sq$dS ~ cd.new.di.sq$dS); abline(0,1)

plot(cd.old.di$dL ~ cd.new.di$dL); abline(0,1)
plot(cd.old.di.s$dL ~ cd.new.di.s$dL); abline(0,1)
plot(cd.old.di.sq$dL ~ cd.new.di.sq$dL); abline(0,1)


plot(cd.old.tri$dS ~ cd.new.tri$dS); abline(0,1)
plot(cd.old.tri.s$dS ~ cd.new.tri.s$dS); abline(0,1)
plot(cd.old.tri.sq$dS ~ cd.new.tri.sq$dS); abline(0,1)

plot(cd.old.tri$dL ~ cd.new.tri$dL); abline(0,1)
plot(cd.old.tri.s$dL ~ cd.new.tri.s$dL); abline(0,1)
plot(cd.old.tri.sq$dL ~ cd.new.tri.sq$dL); abline(0,1)


plot(cd.old.tet$dS ~ cd.new.tet$dS); abline(0,1)
plot(cd.old.tet.s$dS ~ cd.new.tet.s$dS); abline(0,1)
plot(cd.old.tet.sq$dS ~ cd.new.tet.sq$dS); abline(0,1)

plot(cd.old.tet$dS ~ cd.new.tet$dS); abline(0,1)
plot(cd.old.tet$dS ~ cd.new.tet.s$dS); abline(0,1)
plot(cd.old.tet$dS ~ cd.new.tet.sq$dS); abline(0,1)

plot(cd.old.tet$dL ~ cd.new.tet$dL); abline(0,1)
plot(cd.old.tet.s$dL ~ cd.new.tet.s$dL); abline(0,1)
plot(cd.old.tet.sq$dL ~ cd.new.tet.sq$dL); abline(0,1)
