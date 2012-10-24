#spectrum --> rgb
spec2rgb <- function(Y2) {

sens <- read.csv("/Users/chad/Documents/pavo/R_temp/spec.csv", row.names=1)  
	# TODO: references dataset in sys bundle

P2 <- Y2 / sum(Y2)
#P2 <- Y2 / 100

X <- sum(sens[1:401, 9] * P2)
Y <- sum(sens[1:401, 10] * P2)
Z <- sum(sens[1:401, 11] * P2)

xyzmat <- matrix(c(3.240479, -0.969256, 0.055648, -1.537150, 1.875992, -0.204043, -0.498535, 0.041556, 1.057311), nrow = 3)  # what is source of matrix data?

rgb <- xyzmat%*%matrix(c(X, Y, Z)/sum(X, Y, Z), ncol = 1)
#rgb <- xyzmat%*%matrix(c(X, Y, Z), ncol = 1)

rgb[rgb < 0] <- 0
rgb[rgb > 1] <- 1

t(rgb)

}
