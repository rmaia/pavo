library(pavo)

# FIXME: at some point, this line should be removed because all data will be
# generated from this file and we won't have to rely on existing data anymore.
load("R/sysdata.rda")

# bgandilum

d65 <- readxl::read_xls("data-raw/ciedata.xls", skip = 5, col_names = c("wl", "d65"), sheet = "D65")
d65 <- as.rspec(d65, lim = c(300, 700))
d65 <- irrad2flux(d65)
d65 <- procspec(d65, opt = "maximum")

bgandilum$D65 <- d65$d65
bgandilum$ideal <- 1

# transmissiondata

# vissyst

cie2 <- readxl::read_xls("data-raw/ciedata.xls", range = "1931 col observer!A6:D86", col_names = c("wl", "x", "y", "z"))
cie2 <- as.rspec(cie2, lim = c(300, 700), exceed.range = FALSE)
cie2[is.na(cie2)] <- 0

vissyst[, paste0("cie2_", c("X", "Y", "Z"))] <- cie2[, c("x", "y", "z")]

cie10 <- readxl::read_xls("data-raw/ciedata.xls", range = "1964 col observer!A6:D86", col_names = c("wl", "x", "y", "z"))
cie10 <- as.rspec(cie10, lim = c(300, 700), exceed.range = FALSE)
cie10[is.na(cie10)] <- 0

vissyst[, paste0("cie10_", c("X", "Y", "Z")]) <- cie10[, c("x", "y", "z")]

usethis::use_data(
  bgandilum,
  transmissiondata,
  ttvertex,
  vissyst,
  internal = TRUE,
  overwrite = TRUE
)
