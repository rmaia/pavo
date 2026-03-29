library(pavo)

# FIXME: at some point, this line should be removed because all data will be
# generated from this file and we won't have to rely on existing data anymore.
load(file.path("R", "sysdata.rda"))

# bgandilum

d65 <- readxl::read_xls(file.path("data-raw", "ciedata.xls"), skip = 5, col_names = c("wl", "d65"), sheet = "D65")
d65 <- as.rspec(d65, lim = c(300, 700))
d65 <- irrad2flux(d65)
d65 <- procspec(d65, opt = "maximum")

bgandilum$D65 <- d65$d65
bgandilum$ideal <- 1

# transmissiondata

# vissyst

# CIE
cie2 <- readxl::read_xls(
  file.path("data-raw", "ciedata.xls"),
  range = "1931 col observer!A6:D86",
  col_names = c("wl", "x", "y", "z")
)
cie2 <- as.rspec(cie2, lim = c(300, 700), exceed.range = FALSE)
cie2[is.na(cie2)] <- 0

vissyst[, paste0("cie2_", c("X", "Y", "Z"))] <- cie2[, c("x", "y", "z")]

cie10 <- readxl::read_xls(
  file.path("data-raw", "ciedata.xls"),
  range = "1964 col observer!A6:D86",
  col_names = c("wl", "x", "y", "z")
)
cie10 <- as.rspec(cie10, lim = c(300, 700), exceed.range = FALSE)
cie10[is.na(cie10)] <- 0

vissyst[, paste0("cie10_", c("X", "Y", "Z"))] <- cie10[, c("x", "y", "z")]

# Drosophila melanogaster (Sharkey et al. 2020)
dros <- procspec(as.rspec(read.csv(file.path("data-raw", "drosophila_melanogaster_sharkey.csv"))), fixneg = "zero")
vissyst <- merge(vissyst, dros)

# avg.uv & avg.v come from the online appendix of Endler and Mielke (2005).
avgs <- read.table(
  file.path("data-raw", "Endler_Mielke_2005_onlineappendix.txt"),
  skip = 3,
  col.names = c(
    "wl",
    "avg.uv.u", "avg.uv.s", "avg.uv.m", "avg.uv.l",
    "avg.v.u", "avg.v.s", "avg.v.m", "avg.v.l"
  )
)
avgs[, -1] <- vapply(avgs[, -1], function(x) x / 100, FUN.VALUE = numeric(nrow(avgs)))
avgs <- as.rspec(avgs, lim = c(300, 700), interp = FALSE, exceed.range = FALSE)
vissyst[
  c(
    "avg.uv.u", "avg.uv.s", "avg.uv.m", "avg.uv.l",
    "avg.v.u", "avg.v.s", "avg.v.m", "avg.v.l"
  )
] <- avgs[, -1]

# Convert

# Output
usethis::use_data(
  bgandilum,
  transmissiondata,
  ttvertex,
  vissyst,
  internal = TRUE,
  overwrite = TRUE
)
