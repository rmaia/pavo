# TODO(Pierre): Finish coding variables
# TODO(Pierre): Error handling
# TODO(Pierre): Test
# TODO(Pierre): Add documentation

colorvar <- function (filename) {

all.specs <- read.csv(filename, header=T)

output.mat <- matrix (nrow=(dim(all.specs)[2]-1), ncol=12)


B1 <- sapply(all.specs[, 2:dim(all.specs)[2]], sum)  # B1
  output.mat[, 1] <- B1

output.mat[, 2] <- sapply(all.specs[, 2:dim(all.specs)[2]], mean)  # B2

B3 <- sapply(all.specs[, 2:dim(all.specs)[2]], max)  # B3
  output.mat[, 3] <- B3

output.mat[, 4] <- all.specs[apply(all.specs, 2, which.max)[2:dim(all.specs)[2]], 1]  # H1

Redchromamat <- as.matrix(all.specs[306:401, 2:dim(all.specs)[2]])
  Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red
  output.mat [, 5] <- Redchroma

Greenchromamat <- as.matrix(all.specs[211:306, 2:dim(all.specs)[2]])
  Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green
  output.mat [, 6] <- Greenchroma

Bluechromamat <- as.matrix(all.specs[101:211, 2:dim(all.specs)[2]])
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue
  output.mat [, 7] <- Bluechroma

UVchromamat <- as.matrix(all.specs[1:101, 2:dim(all.specs)[2]])
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 8] <- UVchroma

Rmin <- sapply (all.specs[, 2:dim(all.specs)[2]], min)
  output.mat[, 9] <- B3/Rmin # S2

output.mat [, 10] <- B3-Rmin # S6
output.mat [, 11] <- (B3-Rmin)/B1 # S8

Carotchromamat <- as.matrix(all.specs[151:401, 2:dim(all.specs)[2]])
  Carotchroma <- (apply(Carotchromamat,2,sum))/B1 # S9 Carotenoid chroma
  output.mat [, 12] <- Carotchroma

color.var <- as.data.frame(output.mat, row.names=names(all.specs[, 2:dim(all.specs)[2]]))

names(color.var) <- c("B1", "B2", "B3", "H1", "S1 Red", "S1 Green", "S1 Blue", "S1 UV", "S2",
			    "S6", "S8", "S9")
return(color.var)
}







