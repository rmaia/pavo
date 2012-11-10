#' Tristimulus Color Variables
#'
#' The tristimulus color variables calculated by this function are 
#' described in Montgomerie (2006) with corrections included in the README CLR
#' file from the May 2008 distribution of the CLR sofware.
#'
#' Auhtors of research papers that have made use of the \code{colorvar} function
#' or of the \code{summary} for \code{rspec} class objects should reference both 
#' this package and Montgomerie (2006).
#'
#' Description and notes on the measures
#'
#' B1 (Total brightness): Sum of the relative reflectance over the entire spectral
#' range (area under the curve). Frequently used but should be discouraged because
#' values are difficult to compare across studies. B2 is recommended. REF 1-4, 6, 8,
#' 10, 13
#'
#' B2 (Mean brightness): Mean relative reflectance over the entire spectral range.
#' While not the correct way to measure brightness, this is prefered to B1 because 
#' values are easier to compare across studies. REF 5, 11
#'
#' B3 (Intensity): Maximum relative reflectance. Should be used with caution because
#' it is very sensitive to noise near maxima. Should be used only if data has been
#' properly smoothed. REF 1, 7, 9
#' 
#' S1 (Chroma): Relative contribution of a spectral range to the total brightness (B1)
#' S1 is arbitrarily devided in 6 measures of chroma based on the wavelength ranges 
#' normally associated with specific hues. The values are calculated using the 
#' following ranges: S1R(Red) 605nm-lambda max, S1G(Green) 510nm-605nm, S1B(Blue)
#' 400nm-510nm, S1U(UV) lambda min-400nm, S1v(Violet) lambda min-415nm, S1Y(Yellow) 
#' 550nm-625nm. REF 3, 4, 6, 11-13
#'
#' S2 (Spectral saturation): Rmax\Rmin This measure is sensitive to spectral noise.
#' Proper interpretation of this value may be difficult in surfaces with multiple
#' peaks in the range of interest. REF 1
#'
#' S3 (Chroma): Reflectance over the Rmax +- 50nm range divided by B1. Values for peaks 
#' within 50nm of either the minimum r maximum range of the data will not be comparable 
#' since the area under the curve calculated for the area of interest will not always 
#' be based on the same number of wavelength. Therefore, S3 should be interpreted 
#' with caution for peaks in the UV or Red range. REF 13
#'
#' S4 (Spectral purity): |bmaxneg|. PAVO calculates this value by approximating the derivative
#' of the spectral curve. As such, it is very sensitive to noise and should only
#' be considered when data is adequately smoothed. NAs are returned for curves which
#' do not, at any range of wavelength, decrease in intensity. Therefore, reflectance 
#' curves for brwn and red surfaces, for example, should not generate a values.
#' REF 1
#'
#' S5 (Chroma): Similar in design to segment classification measures (see Montgomerie 2006)
#' for details. REF 8
#'
#' S6 (Contrast): Rmax - Rmin. Because it uses both Rmin and Rmax, this measure is very sensitive
#' to spectral noise. REF 7, 9
#' 
#' S7 (Spectral saturation): See Montgomerie (2006) for details. Relatively sensitive 
#' to noise and can be misleading when more than one maxima and/or minima are present.
#' REF 2, 10
#'
#' S8 (Chroma): (Rmax - Rmin)/B2. Because it uses both Rmin and Rmax, this measure is very sensitive
#' to spectral noise. REF 2, 6
#'
#' S9 (Carotenoid chroma): (R450 - R700)/R700. Should only be used when the color 
#' of the surface is clearly due to carotenoid pigmentation and R450 is lower than
#' R700. Could be sensitive to noise. REF 12
#' 
#' S10 (Peaky chroma): (Rmax - Rmin)/B2 x |bmaxneg|. Should be used with properly 
#' smoothed curves. REF 3
#'
#' H1 (Peak wavelength, hue): Lambda Rmax. Sensitive to noisy spectra and may be variable
#' if there are more than one maxima. REF 1, 3-7, 11, 13
#'
#' H2 (Hue): lambda bmaxneg. Should be calculated using smoothed data. REF 4, 6
#'
#' H3 (Hue): lambda Rmid. Sensitive to noisy spectra and may be variable if there are
#' more than one maxima and minima. REF 2, 6, 10
#'
#' H4 (Hue): Similar to S5 see Montgomerie (2006) for details. REF 8
#' 
#' H5 (Hue): lambda bmax. Sensitive to noisy spectra nd may e variable if there are
#' more than one maxima and minima. REF 9
#'
#'References
#'
#' 1- Andersson, S. 1999. Morphology of uv reflectance in a whistling–thrush: Implications for the study
#' of structural colour signalling in birds. Journal of Avian Biology 30:193-204.
#'
#' 2- Andersson, S., J. Örnborg, and M. Andersson. 1998. Ultraviolet sexual dimorphism and assortative
#' mating in blue tits. Proceedings of the Royal Society B 265:445-450.
#'
#' 3- Andersson, S., S. Pryke, J. Örnborg, M. Lawes, and M. Andersson. 2002. Multiple receivers, multiple
#' ornaments, and a trade-off between agonistic and epigamic signaling in a widowbird. American
#' Naturalist 160:683-691.
#'
#' 4- Delhey, K., A. Johnsen, A. Peters, S. Andersson, and B. Kempenaers. 2003. Paternity analysis reveals
#' opposing selection pressures on crown coloration in the blue tit (parus caeruleus). Proceedings
#' of the Royal Society B 270:2057-2063.
#'
#' 5- Keyser, A. and G. Hill. 1999. Condition-dependent variation in the blue-ultraviolet coloration of a
#' structurally based plumage ornament. Proceedings of the Royal Society B 266:771-777.
#'
#' 6- Keyser, A.J. and G. Hill. 2000. Structurally based plumage coloration is an honest signal of quality in
#' male blue grosbeaks. Behavioural Ecology 11:202-209.
#'
#' 7- Ornborg, J., S. Andersson, S. Griffith, and B. Sheldon. 2002. Seasonal changes in a ultraviolet
#' structural colour signal in blue tits, parus caeruleus. Biological Journal of the Linnean Society
#' 76:237-245.
#'
#' 8- Peters, A., A. Denk, K. Delhey, and B. Kempenaers. 2004. Carotenoid-based bill colour as an
#' indicator of immunocompetence and sperm performance in male mallards. Journal of
#' Evolutionary Biology 17:1111-1120.
#'
#' 9- Pryke, S., M. Lawes, and S. Andersson. 2001. Agonistic carotenoid signalling in male red-collared
#' widowbirds: Aggression related to the colour signal of both the territory owner and model
#' intruder. Animal Behaviour 62:695-704.
#'
#' 10- Saks, L., K. Mcgraw, and P. Hörak. 2003. How feather colour reflects its carotenoid content.
#' Functional Ecology 17:555-561.
#'
#' 11- Shawkey, M., A. Estes, L. Sieffereman, and G. Hill. 2003. Nanostructure predicts intraspecific
#' variation in ultraviolet-blue plumage colour. Proceedings of the Royal Society B
#' 270:1455-1460.
#'
#' 12- Siefferman, L. and G. Hill. 2005. Uv-blue structural coloration and competition for nestboxes in male
#' eastern bluebirds. Animal Behaviour 69:67-72.
#'
#' 13- Smiseth, P., J. Örnborg, S. Andersson, and T. Amundsen. 2001. Is male plumage reflectance
#' correlated with paternal care in bluethroats? Behavioural Ecology 12:164-170.
#'
#' 14- Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J., eds. 
#' Bird Coloration. Volume 1 Mechanisms and measuremements. Harvard University Press, Cambridge, Massachusetts.
