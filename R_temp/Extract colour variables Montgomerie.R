# TODO(Pierre): Finish coding S4, S10, H2, H5 variables
# TODO(Pierre): Vectorize loops ?
# TODO(Pierre): Change order to group B, S, and H 
# TODO(Pierre): Error handling
# TODO(Pierre): Test
# TODO(Pierre): Add documentation

colorvar <- function (filename) {

all.specs <- read.csv(filename, header=T)

output.mat <- matrix (nrow=(dim(all.specs)[2]-1), ncol=21)


B1 <- sapply(all.specs[, 2:dim(all.specs)[2]], sum)  # B1
  output.mat[, 1] <- B1

output.mat[, 2] <- sapply(all.specs[, 2:dim(all.specs)[2]], mean)  # B2

B3 <- sapply(all.specs[, 2:dim(all.specs)[2]], max)  # B3
  output.mat[, 3] <- B3

H1 <- all.specs[apply(all.specs, 2, which.max)[2:dim(all.specs)[2]], 1]  # H1
  output.mat[, 4] <- H1

Redchromamat <- as.matrix(all.specs[306:401, 2:dim(all.specs)[2]]) # red 605-700nm inclusive
  Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red
  output.mat [, 5] <- Redchroma

Greenchromamat <- as.matrix(all.specs[211:306, 2:dim(all.specs)[2]]) # green 510-605nm inlusive
  Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green
  output.mat [, 6] <- Greenchroma

Bluechromamat <- as.matrix(all.specs[101:211, 2:dim(all.specs)[2]]) # blue 400-510nm inclusive
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue
  output.mat [, 7] <- Bluechroma

UVchromamat <- as.matrix(all.specs[1:101, 2:dim(all.specs)[2]]) # UV 300-400nm inclusive
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 8] <- UVchroma

Rmin <- sapply (all.specs[, 2:dim(all.specs)[2]], min)
  output.mat[, 9] <- B3/Rmin # S2

#  Following are matrices for S5 a,b,c which all use different wl ranges
#  These ranges do not overlap with S1

S5aRmat <- as.matrix(all.specs[326:401, 2:dim(all.specs)[2]])
  S5aR <- apply(S5aRmat,2,sum)
S5aYmat <- as.matrix(all.specs[251:326, 2:dim(all.specs)[2]])
  S5aY <- apply(S5aYmat,2,sum)
S5aGmat <- as.matrix(all.specs[176:251, 2:dim(all.specs)[2]])
  S5aG <- apply(S5aGmat,2,sum)
S5aBmat <- as.matrix(all.specs[101:176, 2:dim(all.specs)[2]])
  S5aB <- apply(S5aBmat,2,sum)

S5a <- ((S5aR-S5aG)^2+(S5aY-S5aB)^2)^0.5
  output.mat[, 10] <- S5a

S5bRmat <- as.matrix(all.specs[306:401, 2:dim(all.specs)[2]])
  S5bR <- apply(S5bRmat,2,sum)
S5bYmat <- as.matrix(all.specs[211:306, 2:dim(all.specs)[2]])
  S5bY <- apply(S5bYmat,2,sum)
S5bGmat <- as.matrix(all.specs[116:211, 2:dim(all.specs)[2]])
  S5bG <- apply(S5bGmat,2,sum)
S5bBmat <- as.matrix(all.specs[21:116, 2:dim(all.specs)[2]])
  S5bB <- apply(S5bBmat,2,sum)

S5b <- ((S5bR-S5bG)^2+(S5bY-S5bB)^2)^0.5
  output.mat[, 11] <- S5b

S5cRmat <- as.matrix(all.specs[301:401, 2:dim(all.specs)[2]])
  S5cR <- apply(S5cRmat,2,sum)
S5cYmat <- as.matrix(all.specs[201:301, 2:dim(all.specs)[2]])
  S5cY <- apply(S5cYmat,2,sum)
S5cGmat <- as.matrix(all.specs[101:201, 2:dim(all.specs)[2]])
  S5cG <- apply(S5cGmat,2,sum)
S5cBmat <- as.matrix(all.specs[1:101, 2:dim(all.specs)[2]])
  S5cB <- apply(S5cBmat,2,sum)

S5c <- ((S5cR-S5cG)^2+(S5cY-S5cB)^2)^0.5
  output.mat[, 12] <- S5b

# Similarly calculated H4a, b, c

H4a <- atan(((S5aY-S5aB)/B1)/((S5aR-S5aG)/B1))
  output.mat[, 13] <- H4a

H4b <- atan(((S5bY-S5bB)/B1)/((S5bR-S5bG)/B1))
  output.mat[, 14] <- H4b

H4c <- atan(((S5cY-S5cB)/B1)/((S5cR-S5cG)/B1))
  output.mat[, 15] <- H4c

# S6, S8, Carotenoid chroma

output.mat [, 16] <- B3-Rmin # S6
output.mat [, 17] <- (B3-Rmin)/B1 # S8

Carotchromamat <- as.matrix(all.specs[151:401, 2:dim(all.specs)[2]])
  Carotchroma <- (apply(Carotchromamat,2,sum))/B1 # S9 Carotenoid chroma
  output.mat [, 18] <- Carotchroma

# H3 

lambdaRmin <- all.specs[apply(all.specs, 2, which.min)[2:dim(all.specs)[2]], 1]  # H1
  Rmid <- as.integer((H1+lambdaRmin)/2)
  output.mat [, 19] <- Rmid

# S7


S7 <- vector("numeric",dim(all.specs)[2]-1)
for (i in 2:dim(all.specs)[2]){
  mid <- as.integer ((which.max(all.specs[, i])+(which.min(all.specs[, i])))/2)
  sum_min_mid <- sum(all.specs[which.min(all.specs[, i]):mid, i])
  sum_mid_max <- sum(all.specs[mid:which.max(all.specs[, i]), i])
  S7[i-1] <- (sum_min_mid - sum_mid_max)/(B1[i-1][[1]])
  
}

output.mat[, 20] <- S7

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

output.mat[, 21] <- S3

color.var <- as.data.frame(output.mat, row.names=names(all.specs[, 2:dim(all.specs)[2]]))

names(color.var) <- c("B1", "B2", "B3", "H1", "S1 Red", "S1 Green", "S1 Blue", "S1 UV", "S2",
			    "S5a", "S5b", "S5c", "H4a", "H4b", "H4c", "S6", "S8", "S9", "H3", "S7", "S3")
return(color.var)
}

# Speed test DO NOT RUN

#ptm <- proc.time()
#colorvar("colorvar.csv")
#proc.time() - ptm





