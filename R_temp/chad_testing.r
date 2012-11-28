#### TESTING #####

install.packages("/Users/chad/Documents/pavo", type='source', repos=NULL)
require(pavo)


pheas <- getspec("~/Documents/School/PhD/Projects/Polyplectron/data/2012-05-10/pheasants_200-1000nm", ext="ttt")


# tests
plot(specs, sel=2:10, col=spec2rgb(specs)[2:10])

spp <- c(NA, rep('afropavo',10), rep('pavomut', 6), rep('pbicalc', 6), rep('pchalc', 3),
         rep('pemph', 9), rep('pgerm', 6))

plot(specs, select=spp=='pbicalc', col='black')

# visual models
vm1 <- vismodel(specs, visual="avg.uv")
tcs1 <- tcs(vm1)
ttplot(tcs1, col=spec2rgb(specs))
