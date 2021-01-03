acuityview <- function(image, obj_dist, obj_width, eye_res){
  
  # Images must currently be square, with dimensions a power of 2
  # TODO: kill this limitation
  if(dim(image)[1] != dim(image)[2] || !is.element(dim(image)[1], 2^c(1:100))){
    stop("Image must be square, and with dimensions a power of 2")
  }
  
  # Image width in degrees
  width_deg <- 57.2958 * (2 * atan(obj_width / obj_dist / 2))
  
  # Image width in pixels
  width_pix <- dim(image)[2]
  
  # Image center
  center <- round(width_pix / 2) + 1
  
  # Create a blur matrix with dimensions equal to the image
  blur <- matrix(NA, nrow = width_pix, ncol = width_pix)
  for (i in 1:width_pix){
    for (j in 1:width_pix) {
      x <- i - center
      y <- j - center
      freq <- round(sqrt(x^2 + y^2)) / width_pix * (width_pix / width_deg)
      mySin <- y / sqrt(x^2 + y^2)
      myCos <- x / sqrt(x^2 + y^2)
      eye_res2 <- eye_res * eye_res /sqrt((eye_res * myCos)^2 + (eye_res * mySin)^2)
      blur[i,j] <- exp(-3.56 * (eye_res2 * freq)^2)
    }
  }
  
  # Force the center pixel to value 1
  blur[center, center] = 1
  blur <<- blur
  
  # Linearise sRGB values
  from_srgb <- function(rgb_dat){
    ifelse(rgb_dat <= 0.04045, rgb_dat / 12.92, ((rgb_dat + 0.055) /(1 + 0.055))^2.4)
  }
  linimg <- image
  linimg[ , , 1] <- apply(linimg[ , , 1], c(1, 2), from_srgb)
  linimg[ , , 2] <- apply(linimg[ , , 2], c(1, 2), from_srgb)
  linimg[ , , 3] <- apply(linimg[ , , 3], c(1, 2), from_srgb)
  # linimg_r <- linimg[,,1]
  # linimg_r[] <- from_srgb(linred)
  
  # 2D Fourier Transform -> blur matrix multiplication -> inverse fourier transform
  # Note: acuityview uses fftwtools::fftw2d(channel, inverse = 0) instead of fft().
  # Need to double-check check base fft() is fair replacement for fftw2d(). The values
  # do differ, but end-result is (visually) very similar.
  fft2d <- function(channel){
    fft <- (1/width_pix) * fft_shift(fft(channel, inverse = FALSE))
    transform <- fft * blur
    ifft <- (1/width_pix) * fft(transform, inverse = TRUE)
    Mod(ifft)
  }
  linimg[ , , 1] <- fft2d(linimg[ , , 1])
  linimg[ , , 2] <- fft2d(linimg[ , , 2])
  linimg[ , , 3] <- fft2d(linimg[ , , 3])
  
  # Transform back to sRGB
  to_srgb <- function(rgb_dat){
    ifelse(rgb_dat <= 0.0031308, rgb_dat * 12.92, (((1 + 0.055) * rgb_dat^(1 / 2.4)) - 0.055))
  }
  # for(i in seq_along(linimg)){
  #   linimg[ , , i] <- apply(linimg[ , , i], c(1, 2), to_srgb)
  # }
  linimg[ , , 1] <- apply(linimg[ , , 1], c(1, 2), to_srgb)
  linimg[ , , 2] <- apply(linimg[ , , 2], c(1, 2), to_srgb)
  linimg[ , , 3] <- apply(linimg[ , , 3], c(1, 2), to_srgb)
  
  # Re-scale to a max of 1 if any values end up > 1
  rescale <- function(channel){
    channel <- (channel - min(channel))/max(channel)
  }
  if(any(linimg[ , , 1] > 1))
     linimg[ , , 1] <- rescale(linimg[ , , 1])
  if(any(linimg[ , , 2] > 1))
    linimg[ , , 2] <- rescale(linimg[ , , 2])
  if(any(linimg[ , , 3] > 1))
    linimg[ , , 3] <- rescale(linimg[ , , 3])
  
  linimg
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