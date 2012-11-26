#### TESTING

install.packages("/Users/chad/Documents/pavo", type='source', repos=NULL)
require(pavo)


specs <- getspec("~/Documents/School/PhD/Projects/Polyplectron/data/2012-05-10/pheasants_200-1000nm", ext="ttt")

plot(specs)


# tests
plot(specs, sel=2:10, col=spec2rgb(specs)[2:10])

spp <- c(NA, rep('afropavo',10), rep('pavomut', 6), rep('pbicalc', 6), rep('pchalc', 3),
         rep('pemph', 9), rep('pgerm', 6))

plot(specs, select=spp=='pbicalc', col='black')




# thoughts
  # should i give the option to just calculate colors as spec2rgb?
  # what if people don't subset the colors appropriately?
  # maybe just give the option of calculating colors with spec2rgb inside of plot.rspec
  # also provide separate function if people want to use the colors

# change default plotting color (heat colors are bad for specs)
