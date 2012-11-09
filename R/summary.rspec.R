#' Tristimulus color variables
#'
#' Extracts all 21 tristimulus color variables described in 
#' Montgomerie (2006). Works with \code{rspec} class objects generated 
#' from the \code{getspec} function or data frames that contain wavelength in
#' the first column (named '\code{wl}') and spectra in subsequent columns.
#'
#' @param specdata (required) Data frame with spectral data. 
#' @param smooth Logical argument to determine if data should be smoothed 
#' before extracting some of the values. When TRUE, uses the loess smoothing to 
#' reduce spectral noise when extracting variables for which bmax and
#' bmaxneg are required (defaults to TRUE). See note. 
#' @param span the degree of smoothing if \code{smooth} is TRUE. Smaller values result
#' in greater smoothing (defaults to 0.2).
#' @param range vector of length=2 indicating the lower and upper wavelength bounds used
#' to calculate variables that refer to a color wheel (S5 and H4) (defaults to c(300,700).
#' @param plot Logical. If TRUE, smooth spectra are plotted for verification of the
#' smoothing parameter.
#' @return A data frame containing 21 variables described in Montgomerie (2006)
#' with spectra name as row names. 
#' @note If minimum wavelength is over 400, UV chroma is not computed.
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
 
summary.rspec <- function (specdata, range=c(300,700), 
                smooth=TRUE, span=0.2, plot=FALSE) {

wl_index <- which(names(specdata)=='wl')
wl <- specdata[,wl_index]
lambdamin <- min(wl)
specdata <- specdata[,-wl_index]

output.mat <- matrix (nrow=(dim(specdata)[2]), ncol=21)

# Three measures of brightness
B1 <- sapply(specdata, sum)

B2 <- sapply(specdata, mean)

B3 <- sapply(specdata, max)

# lambda Rmax hue
H1 <- wl[max.col(t(specdata), ties.method='first')]

Redchromamat <- as.matrix(specdata[which(wl==605):which(wl==700),]) # red 605-700nm inclusive
Redchroma <- as.vector(apply(Redchromamat,2,sum))/B1 # S1 red


Greenchromamat <- as.matrix(specdata[which(wl==510):which(wl==605),]) # green 510-605nm inlusive
Greenchroma <- (apply(Greenchromamat,2,sum))/B1 # S1 green

Bluechromamat <- as.matrix(specdata[which(wl==400):which(wl==510),]) # blue 400-510nm inclusive
  Bluechroma <- (apply(Bluechromamat,2,sum))/B1 # S1 blue

# Spectral saturation
Rmin <- sapply(specdata, min)
S2 <- B3/Rmin #S2

# RM: removed 5a,b,c; replaced for a quantile function
#  Matrices and calculations for S5a,b,c which all use different wl ranges

segmts <- trunc(as.numeric(quantile(range[1]:range[2])))

Q1 <- which(wl==segmts[1]):which(wl==segmts[2])
Q2 <- which(wl==segmts[2]):which(wl==segmts[3])
Q3 <- which(wl==segmts[3]):which(wl==segmts[4])
Q4 <- which(wl==segmts[4]):which(wl==segmts[5])

S5R <- apply(specdata[Q4, ],2,sum)
S5Y <- apply(specdata[Q3, ],2,sum)
S5G <- apply(specdata[Q2, ],2,sum)
S5B <- apply(specdata[Q1, ],2,sum)


S5 <- sqrt((S5R-S5G)^2+(S5Y-S5B)^2)

# Similarly calculated H4a, b, c
H4 <- atan(((S5Y-S5B)/B1)/((S5R-S5G)/B1))

# S6, S8, Carotenoid chroma
S6 <- B3-Rmin # S6

S8  <- (B3-Rmin)/B2 # S8

Carotchroma <- colSums(specdata[which(wl==450):which(wl==700),])/B1 # S9 Carotenoid chroma

# H3 
lambdaRmin <- wl[apply(specdata, 2, which.min)]  # H3
  Rmid <- round((H1+lambdaRmin)/2)

# S7
sum_min_mid <- apply(specdata, 2, function(x) 
                     sum(x[which.min(x):round((which.max(x) + which.min(x))/2)]))
sum_mid_max <- apply(specdata, 2, function(x) 
                     sum(x[round((which.max(x) + which.min(x))/2):which.max(x)]))

S7 <- (sum_min_mid - sum_mid_max)/(B1)


# S3

plus50 <- apply(specdata,2,function(x) min(c(which.max(x)+50,which.max(wl))))
minus50 <- apply(specdata,2,function(x) max(c(which.max(x)-50,which.min(wl))))
pmindex <- 1:dim(specdata)[2]

S3 <- sapply(pmindex, function(x) sum(specdata[minus50[x]:plus50[x],x]))/B1


#Metrics that involve bmax with or without smoothing
data <- specdata[ ,1:dim(specdata)[2]]

if(smooth){
  smoothspecs <- apply(specdata,2, function(x) loess.smooth(wl, x, 
                                    span=span, degree=1, evaluation=length(wl))$y)
  }else{
    smoothspecs <- specdata
    warning('Spectral curves not smoothened - 
    variables that rely on derivatives (S4, S10, H2 and H5) are not meaningful', call.=FALSE)
    }

diffsmooth <- apply(smoothspecs,2,diff)

lambdabmaxneg <- wl[apply(diffsmooth,2,which.min)] #H2
bmaxneg <- abs(apply(diffsmooth,2,min)) #S4
S10 <- S8/bmaxneg #S10
lambdabmax <- wl[apply(diffsmooth,2,which.max)] #H5

  output.mat[, 1] <- B1
  output.mat[, 2] <- B2
  output.mat[, 3] <- B3
  output.mat[, 5] <- Bluechroma
  output.mat[, 6] <- Greenchroma
  output.mat[, 7] <- Redchroma
  output.mat[, 8] <- S2
  output.mat[, 9] <- S3
  output.mat[, 10] <- bmaxneg
  output.mat[, 11] <- S5
  output.mat[, 12] <- S6
  output.mat[, 13] <- S7
  output.mat[, 14] <- S8
  output.mat[, 15] <- Carotchroma
  output.mat[, 16] <- S10 
  output.mat[, 17] <- H1
  output.mat[, 18] <- lambdabmaxneg 
  output.mat[, 19] <- Rmid
  output.mat[, 20] <- H4
  output.mat[, 21] <- lambdabmax


if(lambdamin <= 300){
  lminuv <- 300
  UVchromamat <- as.matrix(specdata[which(wl==lminuv):which(wl==400),])
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 4] <- UVchroma
  }
  
if(lambdamin > 300 & lambdamin < 400){
  warning(paste('Minimum wavelength is', lambdamin,'UV-related variables may not be meaningful'), call.=FALSE)
  lminuv <- lambdamin
  UVchromamat <- as.matrix(specdata[which(wl==lminuv):which(wl==400),]) 
  UVchroma <- (apply(UVchromamat,2,sum))/B1 # S1 UV
  output.mat [, 4] <- UVchroma
  }

color.var <- data.frame(output.mat, row.names=names(specdata))

names(color.var) <- c("B1", "B2", "B3", "S1.UV", "S1.blue", "S1.green", 
                      "S1.red", "S2", "S3", "S4", "S5", "S6", "S7", "S8", 
                      "S9", "S10", "H1", "H2", "H3", "H4", "H5")

if(plot)
  plot.spec.curves(cbind(data.frame(wl,smoothspecs)))


color.var
}