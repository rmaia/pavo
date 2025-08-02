#' @importFrom stats fft
acuityview_pad <- function(image, obj_dist, obj_width, eye_res) {
  # Square & power-2 check
  square <- dim(image)[1] == dim(image)[2] && log(dim(image)[1]) %% log(2) == 0

  # Zero-pad if not square with dimension power-2
  if (square) {
    image_pad <- image
  } else {
    # Minimum necessary square dimension (power of 2)
    necessary_dim <- ceiling(log(max(dim(image)[1:2])) / log(2))

    # Number of rows and columns necessary for padding (per-edge)
    row_pad <- (necessary_dim - nrow(image)) / 2
    col_pad <- (necessary_dim - ncol(image)) / 2

    # Create & fill padded image
    image_pad <- array(0, dim = c(necessary_dim, necessary_dim, 3))
    for (i in 1:3) {
      image_pad[, , i] <- img_pad(image[, , i], col_pad, row_pad, opt = "pad", pad_fun = median(image))
    }
  }

  # Image width in degrees
  width_deg <- 57.2958 * (2 * atan(obj_width / obj_dist / 2))

  # Image width in pixels
  width_pix <- dim(image_pad)[2]

  # Image center
  center <- round(width_pix / 2) + 1

  # Create an MTF matrix with dimensions equal to the image
  MTF <- matrix(NA, nrow = width_pix, ncol = width_pix)

  # FIXME: This can properly be entirely vectorized without loops (nor *apply())
  for (i in 1:width_pix) {
    for (j in 1:width_pix) {
      x <- i - center
      y <- j - center
      freq <- round(sqrt(x^2 + y^2)) / width_deg
      mySin <- y / sqrt(x^2 + y^2)
      myCos <- x / sqrt(x^2 + y^2)
      eye_res2 <- eye_res * eye_res / sqrt((eye_res * myCos)^2 + (eye_res * mySin)^2)
      MTF[i, j] <- exp(-3.56 * (eye_res2 * freq)^2)
    }
  }

  # Force the center to 1
  MTF[center, center] <- 1

  # Cancel effect of MTF for non-real (padded) image regions
  # Don't think it's necessary, but keeping for now
  # MTF <- img_pad(MTF, col_pad, row_pad, opt = 'MTF')

  # Linearise sRGB values
  from_srgb <- function(rgb_dat) {
    ifelse(rgb_dat <= 0.04045, rgb_dat / 12.92, ((rgb_dat + 0.055) / (1 + 0.055))^2.4)
  }
  # We have to keep the for loop here to avoid conversion to an array that would
  # drop all the custom rimg attributes
  for (i in 1:3) {
    image_pad[, , i] <- from_srgb(image_pad[, , i])
  }

  # 2D Fourier Transform -> MTF matrix multiplication -> inverse fourier transform
  # Note: acuityview uses fftwtools::fftw2d(channel, inverse = 0) instead of fft().
  # Need to double-check check base fft() is fair replacement for fftw2d(). The values
  # do differ, but end-result is (visually) very similar.
  fft2d <- function(channel) {
    forward <- ((1 / width_pix) * fft_shift(fft(channel, inverse = FALSE))) * MTF
    back <- (1 / width_pix) * fft(forward, inverse = TRUE)
    Mod(back)
  }
  for (i in 1:3) {
    image_pad[, , i] <- fft2d(image_pad[, , i])
  }

  # Transform back to sRGB
  to_srgb <- function(rgb_dat) {
    ifelse(rgb_dat <= 0.0031308, rgb_dat * 12.92, (((1 + 0.055) * rgb_dat^(1 / 2.4)) - 0.055))
  }
  for (i in 1:3) {
    image_pad[, , i] <- to_srgb(image_pad[, , i])
  }

  # Crop image back to original dimensions
  if (square) {
    image <- image_pad
  } else {
    for (i in 1:3) {
      image[, , i] <- img_pad(image_pad[, , i], col_pad, row_pad, opt = "crop")
    }
  }

  # Re-scale to a max of 1 if any values end up > 1
  rescale <- function(channel) {
    if (any(channel > 1)) {
      mfac <- (1 - min(channel)) / (max(channel) - min(channel))
      channel <- min(channel) + (channel - min(channel)) * mfac
    }
    channel
  }
  for (i in 1:3) {
    image[, , i] <- rescale(image[, , i])
  }

  image
}

# Rearrange the output of the FFT by moving
# the zero frequency component to the center
fft_shift <- function(input_matrix) {
  rows <- dim(input_matrix)[1]
  cols <- dim(input_matrix)[2]

  # Swap up/down
  rows_half <- ceiling(rows / 2)
  out_mat <- rbind(input_matrix[((rows_half + 1):rows), (1:cols)], input_matrix[(1:rows_half), (1:cols)])

  # Swap left/right
  cols_half <- ceiling(cols / 2)
  out_mat <- cbind(out_mat[1:rows, ((cols_half + 1):cols)], out_mat[1:rows, 1:cols_half])

  out_mat
}

## Padding function  ##
# The control-flow handles cases
# where an uneven number of rows/columns
# need to be added to reach the power-of-2 square
# dimensions. In those cases the image won't
# be exactly centered within the padding, but
# I don't know what else can be done.
img_pad <- function(dat, col_zeros, row_zeros, opt = c("pad", "crop", "MTF"), pad_fun) {
  if (opt == "pad") {
    # Add column padding
    mat_col_low <- matrix(pad_fun, nrow(dat), floor(col_zeros))
    mat_col_high <- matrix(pad_fun, nrow(dat), ceiling(col_zeros))
    out <- cbind(mat_col_low, dat, mat_col_high)

    # Add row padding
    mat_colrow_low <- matrix(pad_fun, floor(row_zeros), ncol(out))
    mat_colrow_high <- matrix(pad_fun, ceiling(row_zeros), ncol(out))
    out2 <- rbind(mat_colrow_low, out, mat_colrow_high)
  }

  if (opt == "crop") {
    # Crop columns
    if (col_zeros %% 1 == 0) {
      out <- dat[, (col_zeros + 1):(ncol(dat) - col_zeros)]
    } else {
      out <- dat[, ceiling(col_zeros):(ncol(dat) - ceiling(col_zeros))]
    }

    # Crop rows
    if (row_zeros %% 1 == 0) {
      out2 <- out[(row_zeros + 1):(nrow(out) - row_zeros), ]
    } else {
      out2 <- out[(ceiling(row_zeros)):(nrow(out) - ceiling(row_zeros)), ]
    }
  }
  # Don't think this is needed, but keeping for now.
  # if (opt == "MTF") {
  #   out2 <- dat
  #   # Rows
  #   out2[1:floor(row_zeros), ] <- 1
  #   out2[(nrow(dat) - ceiling(row_zeros)):nrow(dat), ] <- 1
  #   # Columns
  #   out2[, 1:floor(col_zeros)] <- 1
  #   out2[, (ncol(dat) - ceiling(col_zeros)):ncol(dat)] <- 1
  # }
  out2
}
