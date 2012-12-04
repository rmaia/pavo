# analysis of iridescence

# load spectra
teal <- readRDS("~/Desktop/teal.rda")


# plot results
plot(teal, type='h', varying=seq(10,65,by=5), ylab="Incident angle (º)")


