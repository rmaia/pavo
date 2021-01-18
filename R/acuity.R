#' @importFrom stats fft
#' @importFrom magick image_resize geometry_size_pixels
acuityview <- function(image, obj_dist, obj_width, eye_res){
  
  # Vector of powers of 2 up until realistic maximum possible image dimension. Clumsy.
  pow2 <- 2^c(1:100)
  
  # Square & power-2 check
  square <- dim(image)[1] == dim(image)[2] && is.element(dim(image)[1], pow2)
  
  # if(!square){
  #   stop("Image must be square, and with dimensions a power of 2")
  # }
  
  # Resize to square if not square with dimension power-2    
  if(!square){
    
    # Save original dimensions
    orig_dim <- dim(image)
    
    # Minimum necessary square dimension
    necessary_dim <- max(pow2[min(which(pow2 >= nrow(image)))], pow2[min(which(pow2 >= ncol(image)))]) 
    
    image_pad <- as.rimg(image_resize(rimg2magick(image), 
                                      geometry_size_pixels(width = necessary_dim, 
                                                           height = necessary_dim, 
                                                           preserve_aspect = FALSE)))
  }else{
    image_pad <- image
  }
  
  # Image width in degrees
  width_deg <- 57.2958 * (2 * atan(obj_width / obj_dist / 2))
  
  # Image width in pixels
  width_pix <- dim(image_pad)[2]
  
  # Image center
  center <- round(width_pix / 2) + 1
  
  # Create MTF matrix with dimensions equal to the image
  MTF <- matrix(NA, nrow = dim(image_pad)[2], ncol = dim(image_pad)[1])
  for (i in 1:width_pix){
    for (j in 1:width_pix) {
      x <- i - center
      y <- j - center
      freq <- round(sqrt(x^2 + y^2)) / width_pix * (width_pix / width_deg)
      mySin <- y / sqrt(x^2 + y^2)
      myCos <- x / sqrt(x^2 + y^2)
      eye_res2 <- eye_res * eye_res /sqrt((eye_res * myCos)^2 + (eye_res * mySin)^2)
      MTF[i,j] <- exp(-3.56 * (eye_res2 * freq)^2)
    }
  }
  
  # Force the center to 1
  MTF[center, center] <-  1
  
  # Linearise sRGB values
  from_srgb <- function(rgb_dat){
    ifelse(rgb_dat <= 0.04045, rgb_dat / 12.92, ((rgb_dat + 0.055) /(1 + 0.055))^2.4)
  }
  for(i in 1:3){
    image_pad[ , , i] <- apply(image_pad[ , , i], c(1, 2), from_srgb)
  }
  
  # 2D Fourier Transform -> MTF matrix multiplication -> inverse fourier transform
  # Note: acuityview uses fftwtools::fftw2d(channel, inverse = 0) instead of fft().
  # Need to double-check check base fft() is fair replacement for fftw2d(). The values
  # do differ, but end-result is (visually) very similar.
  fft2d <- function(channel){
    forward <- ((1/width_pix) * fft_shift(fft(channel, inverse = FALSE))) * MTF
    back <- (1/width_pix) * fft(forward, inverse = TRUE)
    Mod(back)
  }
  for(i in 1:3){
    image_pad[ , , i] <- fft2d(image_pad[ , , i])
  }
  
  # Transform back to sRGB
  to_srgb <- function(rgb_dat){
    ifelse(rgb_dat <= 0.0031308, rgb_dat * 12.92, (((1 + 0.055) * rgb_dat^(1 / 2.4)) - 0.055))
  }
  for(i in 1:3){
    image_pad[ , , i] <- apply(image_pad[ , , i], c(1, 2), to_srgb)
  }
  
  # Resize image back to original dimensions
  if(!square){
    image <- as.rimg(image_resize(rimg2magick(image_pad), 
                                  geometry_size_pixels(width = orig_dim[1], 
                                                       height = orig_dim[2], 
                                                       preserve_aspect = FALSE)))
  }else{
    image <- image_pad
  }
  
  # Re-scale to a max of 1 if any values end up > 1
  # TODO: Confirm correct re-scale
  rescale <- function(channel){
    if(any(channel > 1)){
      # chan_range <- range(channel)
      # mult <- (1 - chan_range[1])/(chan_range[2] - chan_range[1])
      # chan_range[1] + (channel - chan_range[1]) * mult
      channel <- (channel - min(channel))/max(channel)
    }
    channel
  }
  for(i in 1:3){
    image[ , , i] <- rescale(image[ , , i])
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