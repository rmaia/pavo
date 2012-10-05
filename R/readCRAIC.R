# READ CRAIC DATA
readCraic <- function(where, decimal=".", wl=c(300:700)) {
  final <- data.frame(wl=wl)
  oldwd <- getwd()
  setwd(where)
  files <- list.files(pattern=".txt")
  for (i in files) {
    dat <- read.table(i, sep="\t", skip=3)
    names(dat) <- c("wavelength", i)
    interp <- approx(dat[,1], dat[,2], xout=wl)$y
    interp <- as.data.frame(interp)
    names(interp) <- i
    names(interp) <- strsplit(names(interp),".txt")
    final <- data.frame(final, interp)
    }
setwd(oldwd)
final
}
