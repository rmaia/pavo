# TODO(Pierre): Error handling
# TODO(Pierre): Test
# TODO(Pierre): Add documentation

#' Tristimulus color variables
#'
#' Extracts all 25 tristimulus color variables described in 
#' Montgomerie (2006). Works with \code{rspec} class objects generated 
#' from the "getspec" function or data frames that contain wavelength in
#' the first column and spectra values in subsequent columns
#'
#' @param all.specs (required) Data frame with spectral data. Will accept 
#' only data ranging from 300-700nm or 320-700nm in 1nm bins.
#' @param smooth Logical argument to determine if data should be smoothed 
#' before extracting some of the values. When TRUE, uses the "lowess" function
#' (f=0.15), to reduce spectra noise and extracts variables for which bmax and
#' bmaxneg are required. This includes only S4, S10, H2, and H5. All other 
#' variables are extracted using non-smoothed data.
#'
#' @return A data frame containing 25 variables described in Montgomerie (2006)
#' with spectra name as row names. 


 
colorvar <- function (all.specs, smooth=FALSE) {

lambdamin <- all.specs[1, 1]

if (lambdamin != 300 & lambdamin != 320) {
  stop ("Minimum wavelength must be 300nm or 320nm")
  }

if (dim(all.specs)[1] != 401 & dim(all.specs)[1] != 381) {
  stop ("Wavelength length must be 401 or 381")
  }

output.mat <- matrix (nrow=(dim(all.specs)[2]-1), ncol=25)

# Three measures of brightness
B1 <- sapply(all.specs[, 2:dim(all.specs)[2]], sum)  # B1
  output.mat[, 1] <- B1

B2 <- sapply(all.specs[, 2:dim(all.specs)[2]], mean)  # B2
  output.mat[, 2] <- B2

B3 <- sapply(all.specs[, 2:dim(all.specs)[2]], max)  # B3
  output.mat[, 3] <- B3

# lambda Rmax hue
H1 <- all.specs[apply(all.specs, 2, which.max)[2:dim(all.specs)[2]], 1]  # H1
  output.mat[, 19] <- H1

# Regularly used chroma scores
if (lambdamin == 300){
  S1Red <- c(306:401)
  S1Green <- c(211:306)
  S1Blue <- c(101:211)
  S1UV <- c(1:101)
}
if (lambdamin == 320) {
S1Red <- c(286:381)
S1Green <- c(191:286)
S1Blue <- c(81:191)
S1UV <- c(1:81)
}

Redchromamat <- as.matrix(all.specs[S1Red, 2:dim(all.specs)[2]]) # red 605-700nm inclusive
  Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red
  output.mat [, 4] <- Redchroma

Greenchromamat <- as.matrix(all.specs[S1Green, 2:dim(all.specs)[2]]) # green 510-605nm inlusive
  Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green
  output.mat [, 5] <- Greenchroma

Bluechromamat <- as.matrix(all.specs[S1Blue, 2:dim(all.specs)[2]]) # blue 400-510nm inclusive
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue
  output.mat [, 6] <- Bluechroma

UVchromamat <- as.matrix(all.specs[S1UV, 2:dim(all.specs)[2]]) # UV 300-400nm inclusive
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 7] <- UVchroma

# Spectral saturation
Rmin <- sapply (all.specs[, 2:dim(all.specs)[2]], min)
  output.mat[, 8] <- B3/Rmin # S2

if (lambdamin == 300){
  S5aRed <- c(326:401)
  S5aYellow <- c(251:326)
  S5aGreen <- c(176:251)
  S5aBlue <- c(101:176)
}

if (lambdamin == 320){
  S5aRed <- c(306:381)
  S5aYellow <- c(231:306)
  S5aGreen <- c(156:231)
  S5aBlue <- c(81:156)
}

#  Matrices and calculations for S5a,b,c which all use different wl ranges
S5aRmat <- as.matrix(all.specs[S5aRed, 2:dim(all.specs)[2]])
  S5aR <- apply(S5aRmat,2,sum)
S5aYmat <- as.matrix(all.specs[S5aYellow, 2:dim(all.specs)[2]])
  S5aY <- apply(S5aYmat,2,sum)
S5aGmat <- as.matrix(all.specs[S5aGreen, 2:dim(all.specs)[2]])
  S5aG <- apply(S5aGmat,2,sum)
S5aBmat <- as.matrix(all.specs[S5aBlue, 2:dim(all.specs)[2]])
  S5aB <- apply(S5aBmat,2,sum)

S5a <- ((S5aR-S5aG)^2+(S5aY-S5aB)^2)^0.5
  output.mat[, 11] <- S5a

if (lambdamin == 300){
  S5bRed <- c(306:401)
  S5bYellow <- c(211:306)
  S5bGreen <- c(116:211)
  S5bBlue <- c(21:116)
}

if (lambdamin == 320){
  S5bRed <- c(286:381)
  S5bYellow <- c(191:286)
  S5bGreen <- c(96:191)
  S5bBlue <- c(1:96)
}

S5bRmat <- as.matrix(all.specs[S5bRed, 2:dim(all.specs)[2]])
  S5bR <- apply(S5bRmat,2,sum)
S5bYmat <- as.matrix(all.specs[S5bYellow, 2:dim(all.specs)[2]])
  S5bY <- apply(S5bYmat,2,sum)
S5bGmat <- as.matrix(all.specs[S5bGreen, 2:dim(all.specs)[2]])
  S5bG <- apply(S5bGmat,2,sum)
S5bBmat <- as.matrix(all.specs[S5bBlue, 2:dim(all.specs)[2]])
  S5bB <- apply(S5bBmat,2,sum)

S5b <- ((S5bR-S5bG)^2+(S5bY-S5bB)^2)^0.5
  output.mat[, 12] <- S5b

S5cRmat <- as.matrix(all.specs[301:401, 2:dim(all.specs)[2]])
  S5cR <- apply(S5cRmat,2,sum)
S5cYmat <- as.matrix(all.specs[201:301, 2:dim(all.specs)[2]])
  S5cY <- apply(S5cYmat,2,sum)
S5cGmat <- as.matrix(all.specs[101:201, 2:dim(all.specs)[2]])
  S5cG <- apply(S5cGmat,2,sum)
S5cBmat <- as.matrix(all.specs[1:101, 2:dim(all.specs)[2]])
  S5cB <- apply(S5cBmat,2,sum)

S5c <- ((S5cR-S5cG)^2+(S5cY-S5cB)^2)^0.5
if (lambdamin == 320) S5c <- NA 
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

if (lambdamin == 300) Carot <- c(151:401)
if (lambdamin == 320) Carot <- c(131:381)

Carotchromamat <- as.matrix(all.specs[Carot, 2:dim(all.specs)[2]])
  Carotchroma <- (apply(Carotchromamat,2,sum))/B1 # S9 Carotenoid chroma
  output.mat [, 17] <- Carotchroma

# H3 
lambdaRmin <- all.specs[apply(all.specs, 2, which.min)[2:dim(all.specs)[2]], 1]  # H1
  Rmid <- as.integer((H1+lambdaRmin)/2)
  output.mat [, 21] <- Rmid

# S7
S7 <- vector("numeric",dim(all.specs)[2]-1)
for (i in 2:dim(all.specs)[2]){
  mid <- as.integer ((which.max(all.specs[, i])+(which.min(all.specs[, i])))/2)
  sum_min_mid <- sum(all.specs[which.min(all.specs[, i]):mid, i])
  sum_mid_max <- sum(all.specs[mid:which.max(all.specs[, i]), i])
  S7[i-1] <- (sum_min_mid - sum_mid_max)/(B1[i-1][[1]])
}

output.mat[, 15] <- S7

# S3
rowmax <- apply(all.specs[, 2:dim(all.specs)[2]], 2, which.max)
plus50 <- rowmax+50
  plus50[plus50 > 401] <- 401
minus50 <- rowmax-50
  minus50[minus50 < 1] <- 1
S3 <- vector("numeric",dim(all.specs)[2]-1)

for (i in 2:dim(all.specs)[2]) {
  S3[i-1] <- sum(all.specs[minus50[i-1]:plus50[i-1], i])/B1[i-1]
}

output.mat[, 9] <- S3

#Metrics that involve bmax with or without smoothing
data <- all.specs[ ,2:dim(all.specs)[2]]

if (smooth == TRUE){
  for (i in 1:dim(data)[2]){
    data[, i] <- lowess(data[ ,i], f=0.15)$y
  }
}

diff.data <- apply(data,2,diff)
lambdabmaxneg <- vector("integer",dim(data)[2]) #H2
lambdabmax <- vector("integer",dim(data)[2]) #H5
bmaxneg <- vector("integer",dim(data)[2]) #S4
S10 <- vector("integer",dim(data)[2]) #S10

for (i in 1:dim(diff.data)[2]){
  lambdabmaxneg[i] <- all.specs[which.min(diff.data[, i]), 1]
  if (min(diff.data[, i]) > 0) 
	lambdabmaxneg[i] <- NA
  bmaxneg[i] <- abs(min(diff.data[, i]))
  if (min(diff.data[, i]) > 0)
	bmaxneg[i] <- NA
  S10[i] <- S8[i]/abs(min(diff.data[, i]))
  if (min(diff.data[, i]) > 0)
	S10[i] <- NA
  lambdabmax[i] <- all.specs[which.max(diff.data[, i]), 1]
  if (max(diff.data[, i]) < 0)
	lambdabmax[i] <- NA
}

output.mat[, 20] <- lambdabmaxneg #H2
output.mat[, 25] <- lambdabmax #H5
output.mat[, 10] <- bmaxneg #S4
output.mat[, 18] <- S10 #S10

color.var <- as.data.frame(output.mat, row.names=names(all.specs[, 2:dim(all.specs)[2]]))

names(color.var) <- c("Total Brightness (B1)", "Mean Brightness (B2)", "Intensity (B3)",
			    "Red chroma (S1 Red)", "Green chroma (S1 Green)", 
			    "Blue chroma (S1 Blue)", "UV chroma (S1 UV)", "Spectral saturation (S2)",
			    "Chroma (S3)", "Spectral purity (S4)", "Chroma (S5a)",
			    "Chroma (S5b)", "Chroma (S5c)", "Contrat (S6)",
			    "Spectral saturation (S7)", "Chroma (S8)", "Carotenoid chroma (S9)", 
			    "Peaky chroma (S10)", "Hue (H1)", "Hue (H2)", "Hue (H3)",
 			    "Hue (H4a)", "Hue (H4b)", "Hue (H4c)", "Hue (H5)")

return(color.var)

}

###################################################################
						# Test area

# Speed test DO NOT RUN

#ptm <- proc.time()
#colorvar(color, smooth=TRUE)
#proc.time() - ptm












