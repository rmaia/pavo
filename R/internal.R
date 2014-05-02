#' @export summary.tcs

loadrgl <- function(){
# load RGL, and attempt install if not found
if (!require('rgl',character.only = TRUE))
  {
  message('package ', dQuote('rgl'), ' not found; attempting install...')
  install.packages('rgl',dep=TRUE)
  
  if(!require('rgl',character.only = TRUE)) 
    stop(dQuote('rgl'), " package is required and could not be installed; please install and try again")
    }
}


# # ttdistcalc <- function(f1,f2,w1,w2,w3,w4){
# #		dq1<-log(f1['u']/f2['u'],base=10)
# #		dq2<-log(f1['s']/f2['s'],base=10)
# #		dq3<-log(f1['m']/f2['m'],base=10)
# #		dq4<-log(f1['l']/f2['l'],base=10)
        # dq1 <- f1['u']-f2['u']
        # dq2 <- f1['s']-f2['s']
        # dq3 <- f1['m']-f2['m']
        # dq4 <- f1['l']-f2['l']
		
		# numer<-	((w1*w2)^2)*((dq4-dq3)^2) + 
				# ((w1*w3)^2)*((dq4-dq2)^2) +
				# ((w1*w4)^2)*((dq3-dq2)^2) +
				# ((w2*w3)^2)*((dq4-dq1)^2) +
				# ((w2*w4)^2)*((dq3-dq1)^2) +
				# ((w3*w4)^2)*((dq2-dq1)^2)
		
		# denom<- ((w1*w2*w3)^2) +
				# ((w1*w2*w4)^2) + 
				# ((w1*w3*w4)^2) +
				# ((w2*w3*w4)^2)	
			
		# as.numeric(sqrt(numer/denom))
		# }

# ttdistcalcachro <- function(f1,f2,w){
        # dq1 <- f1['lum']-f2['lum']
        # dq1 <- as.numeric(dq1)
        # round(abs(dq1/w),7)
		# }

ttdistcalc <- function(f1,f2,w1,w2,w3,w4){
#		dq1<-log(f1['u']/f2['u'],base=10)
#		dq2<-log(f1['s']/f2['s'],base=10)
#		dq3<-log(f1['m']/f2['m'],base=10)
#		dq4<-log(f1['l']/f2['l'],base=10)
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]
        dq3 <- f1[3]-f2[3]
        dq4 <- f1[4]-f2[4]
		
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
		
qn.ttdistcalc <- function(f1,f2, qn1, qn2, n1,n2,n3,n4,v){
#		dq1<-log(f1['u']/f2['u'],base=10)
#		dq2<-log(f1['s']/f2['s'],base=10)
#		dq3<-log(f1['m']/f2['m'],base=10)
#		dq4<-log(f1['l']/f2['l'],base=10)
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]
        dq3 <- f1[3]-f2[3]
        dq4 <- f1[4]-f2[4]
        
        w1 <- sqrt((v^2/n1) + (2/(qn1[1]+qn2[1])))
        w2 <- sqrt((v^2/n2) + (2/(qn1[2]+qn2[2])))
        w3 <- sqrt((v^2/n3) + (2/(qn1[3]+qn2[3])))
        w4 <- sqrt((v^2/n4) + (2/(qn1[4]+qn2[4])))
		
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


didistcalc <- function(f1,f2,w1,w2){
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]

		numer<-	(dq1-dq2)^2
		
		denom<- w1+w2
			
		as.numeric(sqrt(numer/denom))
		}

qn.didistcalc <- function(f1,f2,qn1, qn2, n1,n2,v){
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]

        w1 <- sqrt((v^2/n1) + (2/(qn1[1]+qn2[1])))
        w2 <- sqrt((v^2/n2) + (2/(qn1[2]+qn2[2])))
		
		numer<-	(dq1-dq2)^2
		
		denom<- w1+w2
			
		as.numeric(sqrt(numer/denom))
		}


trdistcalc <- function(f1,f2,w1,w2,w3){
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]
        dq3 <- f1[3]-f2[3]
		
		numer<-	(w1^2*((dq3-dq2)^2)) +
		        (w2^2*((dq3-dq1)^2)) +
		        (w3^2*((dq1-dq2)^2))
		
		denom<- ((w1*w2)^2) +
				((w1*w3)^2) +
				((w2*w3)^2)	
			
		as.numeric(sqrt(numer/denom))
		}

qn.trdistcalc <- function(f1,f2,qn1, qn2, n1,n2,n3,v){
        dq1 <- f1[1]-f2[1]
        dq2 <- f1[2]-f2[2]
        dq3 <- f1[3]-f2[3]
        
        w1 <- sqrt((v^2/n1) + (2/(qn1[1]+qn2[1])))
        w2 <- sqrt((v^2/n2) + (2/(qn1[2]+qn2[2])))
        w3 <- sqrt((v^2/n3) + (2/(qn1[3]+qn2[3])))
		
		numer<-	(w1^2*((dq3-dq2)^2)) +
		        (w2^2*((dq3-dq1)^2)) +
		        (w3^2*((dq1-dq2)^2))
		
		denom<- ((w1*w2)^2) +
				((w1*w3)^2) +
				((w2*w3)^2)	
			
		as.numeric(sqrt(numer/denom))
		}

ttdistcalcachro <- function(f1,f2,w){
        dq1 <- f1[length(f1)]-f2[length(f1)]
        dq1 <- as.numeric(dq1)
        round(abs(dq1/w),7)
		}

qn.ttdistcalcachro <- function(f1,f2,qn1, qn2, n4,v){
        dq1 <- f1[length(f1)]-f2[length(f1)]
        dq1 <- as.numeric(dq1)
        w <- sqrt((v^2/n4) + (2/(qn1[length(qn1)]+qn2[length(qn1)])))
        round(abs(dq1/w),7)
		}


# monodistcalc <- function(f1,f2,w1){
        # dq1 <- f1[1]-f2[1]

		# numer<-	(dq1)^2
		
		# denom<- w1
			
		# as.numeric(sqrt(numer/denom))
		# }

# qn.monodistcalc <- function(f1, f2, qn1, qn2, n1, v){
        # dq1 <- f1[1]-f2[1]

        # w1 <- sqrt((v^2/n1) + (2/(qn1[1]+qn2[1])))
		
		# numer<-	(dq1-dq2)^2
		
		# denom<- w1
			
		# as.numeric(sqrt(numer/denom))
		# }


###################
#SUMMARY VARIABLES#
###################

huedisp <- function(tcsres){
ind=t(combn(nrow(tcsres),2))
apply(ind,1, function(x)	
	 acos((cos(tcsres[x[1],'h.phi'])*cos(tcsres[x[2],'h.phi'])*cos(tcsres[x[1],'h.theta'] -
	 tcsres[x[2],'h.theta'])) + (sin(tcsres[x[1],'h.phi'])*sin(tcsres[x[2],'h.phi'])))
     )
}


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

res.c <- c(centroid,c.vol, colspan.m,colspan.v,hdisp.m, hdisp.v, mean.ra,max.ra)
names(res.c) <- c('centroid.u', 'centroid.s', 'centroid.m', 'centroid.l',
                'c.vol', 'colspan.m', 'colspan.v', 'huedisp.m', 'huedisp.v',
                'mean.ra', 'max.ra')

res.c
}



