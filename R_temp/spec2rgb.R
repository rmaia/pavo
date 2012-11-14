# Convert reflectance spectrum to rgb values
# http://www.cs.rit.edu/~ncs/color/t_spectr.html
# D65 white point
# ref for color matching functions?
# TODO: references dataset in sys bundle

spec2rgb <- function(Y2) {

Y2 <- as.matrix(Y2)

sens <- read.csv("/Users/chad/Documents/pavo/R_temp/spec.csv", row.names=1)  

P2 <- sapply(1:ncol(Y2), function(x) Y2[, x] / sum(Y2[, x]))  # normalize to sum of 1
#P2 <- Y2 / 100

# Calculate human quantum catches (??)
X <- apply(sens[1:401, 9] * P2, 2, sum)
Y <- apply(sens[1:401, 10] * P2, 2, sum)
Z <- apply(sens[1:401, 11] * P2, 2, sum)
XYZ <- rbind(X, Y, Z)

xyzmat <- rbind(c(3.240479, -1.537150, -0.498535),
								c(-0.969256, 1.875992, 0.041556),
								c(0.055648, -0.204043, 1.057311))


XYZ <- sapply(1:ncol(XYZ), function(x) XYZ[, x] / sum(XYZ[, x]))

rgb1 <- sapply(1:ncol(XYZ), function(x) xyzmat%*%as.matrix(XYZ[, x]))
# normalization functions
rgb1[rgb1 < 0] <- 0
rgb1[rgb1 > 1] <- 1

colrs <- rgb(r=rgb1[1,], g=rgb1[2,], b=rgb1[3,])

colrs

}
