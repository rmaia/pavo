# TODO(Pierre): Vectorize loops ?
# TODO(Pierre): Error handling
# TODO(Pierre): Test
# TODO(Pierre): Add documentation

colorvar <- function (all.specs,smooth=FALSE) {

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
Redchromamat <- as.matrix(all.specs[306:401, 2:dim(all.specs)[2]]) # red 605-700nm inclusive
  Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red
  output.mat [, 4] <- Redchroma

Greenchromamat <- as.matrix(all.specs[211:306, 2:dim(all.specs)[2]]) # green 510-605nm inlusive
  Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green
  output.mat [, 5] <- Greenchroma

Bluechromamat <- as.matrix(all.specs[101:211, 2:dim(all.specs)[2]]) # blue 400-510nm inclusive
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue
  output.mat [, 6] <- Bluechroma

UVchromamat <- as.matrix(all.specs[1:101, 2:dim(all.specs)[2]]) # UV 300-400nm inclusive
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 7] <- UVchroma

# Spectral saturation
Rmin <- sapply (all.specs[, 2:dim(all.specs)[2]], min)
  output.mat[, 8] <- B3/Rmin # S2

#  Matrices and calculations for S5a,b,c which all use different wl ranges
S5aRmat <- as.matrix(all.specs[326:401, 2:dim(all.specs)[2]])
  S5aR <- apply(S5aRmat,2,sum)
S5aYmat <- as.matrix(all.specs[251:326, 2:dim(all.specs)[2]])
  S5aY <- apply(S5aYmat,2,sum)
S5aGmat <- as.matrix(all.specs[176:251, 2:dim(all.specs)[2]])
  S5aG <- apply(S5aGmat,2,sum)
S5aBmat <- as.matrix(all.specs[101:176, 2:dim(all.specs)[2]])
  S5aB <- apply(S5aBmat,2,sum)

S5a <- ((S5aR-S5aG)^2+(S5aY-S5aB)^2)^0.5
  output.mat[, 11] <- S5a

S5bRmat <- as.matrix(all.specs[306:401, 2:dim(all.specs)[2]])
  S5bR <- apply(S5bRmat,2,sum)
S5bYmat <- as.matrix(all.specs[211:306, 2:dim(all.specs)[2]])
  S5bY <- apply(S5bYmat,2,sum)
S5bGmat <- as.matrix(all.specs[116:211, 2:dim(all.specs)[2]])
  S5bG <- apply(S5bGmat,2,sum)
S5bBmat <- as.matrix(all.specs[21:116, 2:dim(all.specs)[2]])
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
  output.mat[, 13] <- S5b

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

Carotchromamat <- as.matrix(all.specs[151:401, 2:dim(all.specs)[2]])
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

if (smooth=TRUE){
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
  S10[i] <- S8[i]\abs(min(diff.data[, i]))
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
			    "Spectral saturation (S7), "Chroma (S8)", "Carotenoid chroma (S9)", 
			    "Peaky chroma (S10)", "Hue (H1)", "Hue (H2)", "Hue (H3)",
 			    "Hue (H4a)", "Hue (H4b)", "Hue (H4c)", "Hue (H5)")
return(color.var)
}

# Speed test DO NOT RUN

#ptm <- proc.time()
#colorvar("colorvar.csv")
#proc.time() - ptm





