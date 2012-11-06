#' Tristimulus color variables
#'
#' Extracts all 25 tristimulus color variables described in 
#' Montgomerie (2006). Works with \code{rspec} class objects generated 
#' from the \code{getspec} function or data frames that contain wavelength in
#' the first column and spectra values in subsequent columns.
#'
#' @param all.specs (required) Data frame with spectral data. Will accept 
#' only data ranging from 300-700nm or 320-700nm in 1nm bins.
#' @param smooth Logical argument to determine if data should be smoothed 
#' before extracting some of the values. When TRUE, uses the "lowess" function
#' (f=0.15), to reduce spectra noise and extracts variables for which bmax and
#' bmaxneg are required. See note.
#' @return A data frame containing 25 variables described in Montgomerie (2006)
#' with spectra name as row names. 
#' @note If data range is 320-700nm, S5c, and H4c are not computed.
#' @note Variables which compute bmax and bmaxneg should be used with caution.
#' For example, S4 was designed to evaluate the steepest negative slope of a
#' UV peak. 
#' Therefore, it is not relevant for other spectral curves. See Montgomerie
#' (2006) for details.
#' @note Smoothing affects only S4, S10, H2, and H5 calculation. All other 
#' variables are extracted using non-smoothed data. Effects of this option can be
#' checked by comparing two outputs using \code{match}.
#' @export
#' @author Pierre-Paul Bitton \email{bittonp@@windsor.ca}
#' @references Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J., eds. 
#' Bird Coloration. Volume 1 Mechanisms and measuremements. Harvard University Press, Cambridge, Massachusetts.
 
colorvar2 <- function (all.specs, smooth=TRUE, smooth.f=0.15) {

wl_index <- which(names(all.specs)=='wl')
wl <- all.specs[,wl_index]
lambdamin <- min(wl)
all.specs <- all.specs[,-wl_index]

if(lambdamin <= 300){
  lminuv <- 300
  lminuv320 <- 320
  }
  
if(lambdamin > 300){
  warning('Minimum wavelength is greater than 300 - some UV-related variables are not meaningful')
  lminuv <- lambdamin
  lminuv320 <- 320
  }

if(lambdamin > 320){
  warning('Minimum wavelength is greater than 320 - some UV-related variables are not meaningful')
  lminuv320 <- lambdamin
  }
 

  
output.mat <- matrix (nrow=(dim(all.specs)[2]), ncol=25)

# Three measures of brightness
B1 <- sapply(all.specs, sum)
  output.mat[, 1] <- B1

B2 <- sapply(all.specs, mean)
  output.mat[, 2] <- B2

B3 <- sapply(all.specs, max)
  output.mat[, 3] <- B3

# lambda Rmax hue
H1 <- wl[max.col(t(all.specs), ties.method='first')]
  output.mat[, 19] <- H1


Redchromamat <- as.matrix(all.specs[which(wl==605):which(wl==700),]) # red 605-700nm inclusive
Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red
  output.mat [, 4] <- Redchroma


Greenchromamat <- as.matrix(all.specs[which(wl==510):which(wl==605),]) # green 510-605nm inlusive
Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green
  output.mat [, 5] <- Greenchroma

Bluechromamat <- as.matrix(all.specs[which(wl==400):which(wl==510),]) # blue 400-510nm inclusive
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue
  output.mat [, 6] <- Bluechroma

UVchromamat <- as.matrix(all.specs[which(wl==lminuv):which(wl==400),]) # UV 300-400nm inclusive
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 7] <- UVchroma

# Spectral saturation
Rmin <- sapply(all.specs, min)
  output.mat[, 8] <- B3/Rmin # S2

#  Matrices and calculations for S5a,b,c which all use different wl ranges
S5aR <- apply(all.specs[which(wl==625):which(wl==700),],2,sum)
S5aY <- apply(all.specs[which(wl==550):which(wl==625),],2,sum)
S5aG <- apply(all.specs[which(wl==475):which(wl==550),],2,sum)
S5aB <- apply(all.specs[which(wl==400):which(wl==475),],2,sum)


S5a <- sqrt((S5aR-S5aG)^2+(S5aY-S5aB)^2)
  output.mat[, 11] <- S5a

S5bR <- apply(all.specs[which(wl==605):which(wl==700),],2,sum)
S5bY <- apply(all.specs[which(wl==510):which(wl==605),],2,sum)
S5bG <- apply(all.specs[which(wl==415):which(wl==510),],2,sum)
S5bB <- apply(all.specs[which(wl==lminuv320):which(wl==415),],2,sum)

S5b <- ((S5bR-S5bG)^2+(S5bY-S5bB)^2)^0.5
  output.mat[, 12] <- S5b

S5cR <- apply(all.specs[which(wl==600):which(wl==700),],2,sum)
S5cY <- apply(all.specs[which(wl==500):which(wl==600),],2,sum)
S5cG <- apply(all.specs[which(wl==400):which(wl==500),],2,sum)
S5cB <- apply(all.specs[which(wl==lminuv):which(wl==400),],2,sum)

S5c <- ((S5cR-S5cG)^2+(S5cY-S5cB)^2)^0.5
 output.mat[, 13] <- S5c

# Similarly calculated H4a, b, c
H4a <- atan(((S5aY-S5aB)/B1)/((S5aR-S5aG)/B1))
  output.mat[, 22] <- H4a

H4b <- atan(((S5bY-S5bB)/B1)/((S5bR-S5bG)/B1))
  output.mat[, 23] <- H4b

H4c <- atan(((S5cY-S5cB)/B1)/((S5cR-S5cG)/B1))
  output.mat[, 24] <- H4c

# S6, S8, Carotenoid chroma
output.mat [, 14] <- B3-Rmin # S6

S8  <- (B3-Rmin)/B2 # S8
  output.mat [, 16]<- S8

Carotchroma <- colSums(all.specs[which(wl==450):which(wl==700),])/B1 # S9 Carotenoid chroma
  output.mat [, 17] <- Carotchroma

# H3 
lambdaRmin <- wl[apply(all.specs, 2, which.min)]  # H1
  Rmid <- round((H1+lambdaRmin)/2)
  output.mat [, 21] <- Rmid

# S7
sum_min_mid <- apply(all.specs, 2, function(x) 
                     sum(x[which.min(x):round((which.max(x) + which.min(x))/2)]))
sum_mid_max <- apply(all.specs, 2, function(x) 
                     sum(x[round((which.max(x) + which.min(x))/2):which.max(x)]))

S7 <- (sum_min_mid - sum_mid_max)/(B1)

output.mat[, 15] <- S7

# S3

plus50 <- apply(all.specs,2,function(x) min(c(which.max(x)+50,which.max(wl))))
minus50 <- apply(all.specs,2,function(x) max(c(which.max(x)-50,which.min(wl))))
pmindex <- 1:dim(all.specs)[2]

S3 <- sapply(pmindex, function(x) sum(all.specs[minus50[x]:plus50[x],x]))/B1
  output.mat[, 9] <- S3

#Metrics that involve bmax with or without smoothing
data <- all.specs[ ,1:dim(all.specs)[2]]

if(smooth){
  smoothspecs <- apply(all.specs,2, function(x) loess.smooth(wl, x, 
                                    span=0.2, degree=1, evaluation=length(wl))$y)
  }else{
    smoothspecs <- all.specs
    warning('Spectral curves not smoothened - 
    variables that rely on derivatives (S4, S10, H2 and H5) are not meaningful', call.=FALSE)
    }

diffsmooth <- apply(smoothspecs,2,diff)
lambdabmaxneg <- wl[apply(diffsmooth,2,which.min)]
bmaxneg <- abs(apply(diffsmooth,2,min))
S10 <- S8/bmaxneg
lambdabmax <- wl[apply(diffsmooth,2,which.max)]


output.mat[, 20] <- lambdabmaxneg #H2
output.mat[, 25] <- lambdabmax #H5
output.mat[, 10] <- bmaxneg #S4
output.mat[, 18] <- S10 #S10

if(lambdamin > 320){
  output.mat[, 7] <- NA
  output.mat[, 12] <- NA
  output.mat[, 13] <- NA
}

color.var <- data.frame(output.mat, row.names=names(all.specs))

names(color.var) <- c("B1", "B2", "B3", "S1.red", "S1.green", "S1.blue", 
                      "S1.UV", "S2", "S3", "S4", "S5a", "S5b", "S5c", 
                      "S6", "S7", "S8", "S9", "S10", "H1", "H2", "H3",
                      "H4a", "H4b", "H4c", "H5")

color.var
}