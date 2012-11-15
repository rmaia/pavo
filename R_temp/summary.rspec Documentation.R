#' @return A data frame containing 23 variables described in Montgomerie (2006)
#' with spectra name as row names. 
#' @return The tristimulus color variables calculated by this function are 
#' described in Montgomerie (2006) with corrections included in the README CLR
#' file from the May 2008 distribution of the CLR sofware. Auhtors should reference 
#' both this package and Montgomerie (2006).
#'
#' @return Description and notes on the measures
#'
#' B1 (Total brightness): Sum of the relative reflectance over the entire spectral
#' range (area under the curve). Frequently used but should be discouraged because
#' values are difficult to compare across studies (B2 is preferred). REF 1-4, 6, 8,
#' 10, 13
#'
#' B2 (Mean brightness): Mean relative reflectance over the entire spectral range.
#' This is prefered to B1 since values are easier to compare across studies. REF 5, 11
#'
#' B3 (Intensity): Maximum relative reflectance (Reflectance at wavelength of maximum
#' reflectance). Note that may be sensitive to noise near the peak. REF 1, 7, 9
#' 
#' S1 (Chroma): Relative contribution of a spectral range to the total brightness (B1)
#' S1 is arbitrarily devided in 6 measures of chroma based on the wavelength ranges 
#' normally associated with specific hues. The values are calculated using the 
#' following ranges: S1U (UV, if applicable): lambda min-400nm; 
#' S1v (Violet) lambda min-415nm; S1B (Blue) 400nm-510nm; S1G (Green) 510nm-605nm;
#' S1Y (Yellow) 550nm-625nm; S1R (Red) 605nm-lambda max. REF 3, 4, 6, 11-13
#'
#' S2 (Spectral saturation): Rmax/Rmin This measure is sensitive to spectral noise.
#' Proper interpretation of this value may be difficult for spectra with multiple
#' peaks in the range of interest. REF 1
#'
#' S3 (Chroma): Reflectance over the Rmax +- 50nm range divided by B1. Values for peaks 
#' within 50nm of either the minimum or maximum range of the data will not be comparable 
#' since the area under the curve calculated for the area of interest will not always 
#' be based on the same number of wavelength. Therefore, S3 should be interpreted 
#' with caution for peaks in the UV or Red range. REF 13
#'
#' S4 (Spectral purity): |bmaxneg| , calculated by approximating the derivative
#' of the spectral curve. As such, it is very sensitive to noise and should only
#' be considered when data is adequately smoothed. NAs are returned for curves which
#' do not, at any range of wavelength, decrease in intensity. Therefore, reflectance 
#' curves for brwn and red surfaces, for example, should not generate a values. REF 1
#'
#' S5 (Chroma): Similar in design to segment classification measures (see Montgomerie 2006)
#' for details. REF 8
#'
#' S6 (Contrast): Rmax - Rmin. Because it uses both Rmin and Rmax, this measure may be
#' sensitive to spectral noise. REF 7, 9
#' 
#' S7 (Spectral saturation): Relative reflectance between the area around the peak with
#' reflectance equal to or larger to half of that of the peak (an approximation to the
#' full-width at half maxima. See Montgomerie (2006) for details). Somewhat sensitive 
#' to noise and can be misleading when more than one maxima and/or minima are present.
#' REF 2, 10
#'
#' S8 (Chroma): (Rmax - Rmin)/B2. Because it uses both Rmin and Rmax, this measure may be
#' sensitive to spectral noise. REF 2, 6
#'
#' S9 (Carotenoid chroma): (R450 - R700)/R700. Should only be used when the color 
#' of the surface is clearly due to carotenoid pigmentation and R450 is lower than
#' R700. Could be sensitive to noise. REF 12
#' 
#' S10 (Peaky chroma): (Rmax - Rmin)/B2 x |bmaxneg|. Should be used with properly 
#' smoothed curves. REF 3
#'
#' H1 (Peak wavelength, hue): Wavelength of maximum reflectance. May be sensitive to noise
#' and may be variable if there is more than one maxima. REF 1, 3-7, 11, 13
#'
#' H2 (Hue): Wavelength at bmaxneg. Should be calculated using smoothed data. REF 4, 6
#'
#' H3 (Hue): Wavelength at Rmid. Sensitive to noisy spectra and may be variable if there are
#' more than one maxima and minima. REF 2, 6, 10
#'
#' H4 (Hue): Similar in design to segment classification measures see Montgomerie \
#' (2006) for details. REF 8
#' 
#' H5 (Hue): Wavelength at bmax. Sensitive to noise and may be variable if there is
#' more than one maxima and minima. REF 9
#'
#'References

