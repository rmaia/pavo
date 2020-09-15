library(pavo)

# FIXME: at some point, this line should be removed because all data will be
# generated from this file and we won't have to rely on existing data anymore.
load("R/sysdata.rda")
bgandilum <- bgandilum[, c("wl", "bluesky", "forestshade", "green")]
vissyst <- vissyst[, !grepl("cie", colnames(vissyst))]

# bgandilum

d65 <- readxl::read_xls("data-raw/ciedata.xls", skip = 5, col_names = c("wl", "D65"), sheet = "D65")
d65 <- as.rspec(d65)
d65 <- procspec(d65, opt = "maximum")

bgandilum <- merge(bgandilum, d65, all = TRUE)
bgandilum[is.na(bgandilum)] <- 0

# transmissiondata

# vissyst

cie2 <- readxl::read_xls(
  "data-raw/ciedata.xls",
  range = "1931 col observer!A6:D86",
  col_names = c("wl", paste0("cie2_", c("X", "Y", "Z")))
)
cie2 <- as.rspec(cie2)

vissyst <- merge(vissyst, cie2, all = TRUE)

cie10 <- readxl::read_xls(
  "data-raw/ciedata.xls",
  range = "1964 col observer!A6:D86",
  col_names = c("wl", paste0("cie10_", c("X", "Y", "Z")))
)
cie10 <- as.rspec(cie10)

vissyst <- merge(vissyst, cie10, all = TRUE)

vissyst[is.na(vissyst)] <- 0

usethis::use_data(
  bgandilum,
  transmissiondata,
  ttvertex,
  vissyst,
  internal = TRUE,
  overwrite = TRUE
)
