#' @export summary.tcs

# summary.rspec <- function(spcs){
	# cat('\nSpectra data frame\n')
	# cat('Number of spectra:',dim(spcs)[2]-1,'\n')
	# cat('Wavelength range:',min(spcs$wl),'to',max(spcs$wl),'nm\n\n')
	# cat('Spectra names (showing first 10):\n')
	# cat(head(names(spcs[,-which(names(spcs)=='wl')]),10),'\n')
	# cat('\n')
# }


summary.tcs <- function(tcsres, by=NULL){
if(!is.null(by)){
	
	if(length(by)==1){
	by.many <- by
	by <- rep(1:(dim(tcsres)[1]/by),each=by)
	by <- factor(by,labels=row.names(tcsres)[seq(1,length(row.names(tcsres)),by=by.many)])
    }

  by <- factor(by)
  res.c <- data.frame(t(sapply(levels(by),function(z)tcssum(tcsres[which(by==z),]))))
  row.names(res.c) <- levels(by)
	
  }else{
	res.c <- data.frame(t(tcssum(tcsres)))
	row.names(res.c) <- 'all.points'
    }

if(NA %in% res.c$cvol)
  warning('Not enough points to calculate volume', call.=FALSE)

res.c
}








huedisp <- function(tcsres){
ind=t(combn(nrow(tcsres),2))
apply(ind,1, function(x)	
	 acos((cos(tcsres[x[1],'h.phi'])*cos(tcsres[x[2],'h.phi'])*cos(tcsres[x[1],'h.theta'] -
	 tcsres[x[2],'h.theta'])) + (sin(tcsres[x[1],'h.phi'])*sin(tcsres[x[2],'h.phi'])))
     )
}


ttdistcalc <- function(f1,f2,w1,w2,w3,w4){
#		dq1<-log(f1['u']/f2['u'],base=10)
#		dq2<-log(f1['s']/f2['s'],base=10)
#		dq3<-log(f1['m']/f2['m'],base=10)
#		dq4<-log(f1['l']/f2['l'],base=10)
        dq1 <- f1['u']-f2['u']
        dq2 <- f1['s']-f2['s']
        dq3 <- f1['m']-f2['m']
        dq4 <- f1['l']-f2['l']
		
		numer<-	((w1*w2)^2)*((dq4-dq3)^2) + 
				((w1*w3)^2)*((dq4-dq2)^2) +
				((w1*w4)^2)*((dq3-dq2)^2) +
				((w2*w3)^2)*((dq4-dq1)^2) +
				((w2*w4)^2)*((dq3-dq1)^2) +
				((w3*w4)^2)*((dq2-dq1)^2)
		
		denom<- ((w1*w2*w3)^2) +
				((w1*w2*w4)^2) + 
				((w1*w3*w4)^2) +
				((w2*w3*w4)^2)	
			
		as.numeric(sqrt(numer/denom))
		}


ttdistcalcachro <- function(f1,f2,w){
        dq1 <- f1['lum']-f2['lum']
        dq1 <- as.numeric(dq1)
        round(abs(dq1/w),7)
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

if(nrow(tcsres)>3)
     {
     c.vol <- convhulln(tcsres[,c('x','y','z')],"FA")$vol
     }else{
       c.vol<-NA}

# hue disparity

hdisp.m <- mean(huedisp(tcsres))
hdisp.v <- var(huedisp(tcsres))

# summary of achieved chroma

mean.ra <- mean(tcsres$r.achieved)
max.ra  <-  max(tcsres$r.achieved)

res.c <- c(centroid,c.vol, colspan.m,colspan.v,mean.ra,max.ra)
names(res.c) <- c('centroid.u', 'centroid.s', 'centroid.m', 'centroid.l',
                'c.vol', 'colspan.m', 'colspan.v', 'mean.ra', 'max.ra')

res.c
}


