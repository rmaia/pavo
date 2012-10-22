#' @export summary.vismodel
#' @export summary.spec

summary.vismodel<-function(x){
	lapply(x,colMeans)
}

summary.spec <- function(spcs){
	cat('\nSpectra data frame\n')
	cat('Number of spectra:',dim(spcs)[2]-1,'\n')
	cat('Wavelength range:',min(spcs$wl),'to',max(spcs$wl),'nm\n\n')
	cat('Spectra names (showing first 10):\n')
	cat(head(names(spcs[,-which(names(spcs)=='wl')]),10),'\n')
	cat('\n')
}



huedisp <- function(tcsres){
ind=t(combn(nrow(tcsres),2))
apply(ind,1, function(x)	
	 acos((cos(tcsres[x[1],'h.phi'])*cos(tcsres[x[2],'h.phi'])*cos(tcsres[x[1],'h.theta'] -
	 tcsres[x[2],'h.theta'])) + (sin(tcsres[x[1],'h.phi'])*sin(tcsres[x[2],'h.phi'])))
     )
}


###################
#SUMMARY VARIABLES#
###################

tcssum <- function(tcsres){
# centroid
centroid <- colMeans(tcsres[c('u','s','m','l')])

# color span
colspan.m <- mean(dist(tcsres[,c('x','y','z')]))
colspan.v <- var(dist(tcsres[,c('x','y','z')]))

# color volume

#if(nrow(tcsres)>3)
#     {
     c.vol <- convhulln(tcsres[,c('x','y','z')],"FA")$vol
#     }else{
#     	warning('Not enough color points to calculate volume (min 4)', call.=FALSE)
#     	Color.vol<-NA}

# hue disparity

hdisp.m <- mean(huedisp(tcsres))
hdisp.v <- var(huedisp(tcsres))

# summary of achieved chroma

mean.ra <- mean(tcsres$r.achieved)
max.ra  <-  max(tcsres$r.achieved)

res.c <- c(centroid,colspan.m,colspan.v,mean.ra,max.ra)
names(res.c) <- c('centroid.u', 'centroid.s' ,'centroid.m' ,'centroid.l' ,
                'colspan.m', 'colspan.v', 'mean.ra', 'max.ra')

res.c
}


