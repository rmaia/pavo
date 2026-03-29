# Colourimetric variables

Calculates all 23 colourimetric variables reviewed in Montgomerie
(2006).

## Usage

``` r
# S3 method for class 'rspec'
summary(object, subset = FALSE, lim = NULL, wlmin = NULL, wlmax = NULL, ...)
```

## Arguments

- object:

  (required) a data frame, possibly an object of class `rspec`, with a
  column with wavelength data, named 'wl', and the remaining column
  containing spectra to process.

- subset:

  Either `FALSE` (the default), `TRUE`, or a character vector. If
  `FALSE`, all variables calculated are returned. If `TRUE`, only a
  subset of the complete output (composed of B2, S8 and H1; the
  variables described in Andersson and Prager 2006) are returned.
  Finally, a user-specified string of variable names can be used in
  order to filter and show only those variables.

- lim:

  The range of wavelengths used in calculations. The default is to use
  the entire range in the `rspec` object (typically equivalent to
  `lim = c(300, 700)`).

- wlmin, wlmax:

  Deprecated. Use the `lim` argument instead.

- ...:

  class consistency (ignored)

## Value

A data frame containing either 23 or 5 (`subset = TRUE`) variables
described in Montgomerie (2006) with spectra name as row names. The
colorimetric variables calculated by this function are described in
Montgomerie (2006) with corrections included in the README CLR file from
the May 2008 distribution of the CLR software. Authors should reference
both this package,Montgomerie (2006), and the original reference(s).
Description and notes on the measures:

B1 (Total brightness): Sum of the relative reflectance over the entire
spectral range (area under the curve). Frequently used but should be
discouraged because values are difficult to compare across studies (B2
is preferred). REF 1-3, 7, 9-11, 13

B2 (Mean brightness): Mean relative reflectance over the entire spectral
range. This is preferred to B1 since values are easier to compare across
studies. REF 4, 12

B3 (Intensity): Maximum relative reflectance (Reflectance at wavelength
of maximum reflectance). Note that may be sensitive to noise near the
peak. REF 1, 5, 6

S1 (Chroma): Relative contribution of a spectral range to the total
brightness (B1) S1 is arbitrarily divided in 6 measures of chroma based
on the wavelength ranges normally associated with specific hues. The
values are calculated using the following ranges: S1U (UV, if
applicable): lambda min-400nm; S1V (Violet) lambda min-415nm; S1B (Blue)
400nm-510nm; S1G (Green) 510nm-605nm; S1Y (Yellow) 550nm-625nm; S1R
(Red) 605nm-lambda max. REF 2, 7, 8, 11-13

S2 (Spectral saturation): Rmax/Rmin This measure is sensitive to
spectral noise. Proper interpretation of this value may be difficult for
spectra with multiple peaks in the range of interest. REF 1

S3 (Chroma): Reflectance over the Rmax +- 50nm range divided by B1.
Values for peaks within 50nm of either the minimum or maximum range of
the data will not be comparable since the area under the curve for the
area of interest will not always be based on the same wavelength range.
Therefore, S3 should be interpreted with caution for peaks in the UV or
Red range. REF 11

S4 (Spectral purity): \|bmaxneg\| , calculated by approximating the
derivative of the spectral curve. As such, it is very sensitive to noise
and should only be considered when data is adequately smoothed. NAs are
returned for curves which do not, at any range of wavelength, decrease
in intensity. Therefore, reflectance curves for brown and red surfaces,
for example, should not generate a values. REF 1

S5 (Chroma): Similar in design to segment classification measures (see
Montgomerie 2006 for details). REF 10

S6 (Contrast): Rmax - Rmin. Because it uses both Rmin and Rmax, this
measure may be sensitive to spectral noise. REF 5, 6

S7 (Spectral saturation): Difference between the relative reflectance
before and after the wavelength at which reflectance is halfway between
its minimum (Rmin) and its maximum (Rmax). Somewhat sensitive to noise
and can be misleading when more than one maxima and/or minima are
present. REF 3, 9

S8 (Chroma): (Rmax - Rmin)/B2. Because it uses both Rmin and Rmax, this
measure may be sensitive to spectral noise. REF 3, 13

S9 (Carotenoid chroma): (R700 - R450)/R700. Should only be used when the
colour of the surface is clearly due to carotenoid pigmentation and R450
is lower than R700. Could be sensitive to noise. REF 8

S10 (Peaky chroma): (Rmax - Rmin)/B2 x \|bmaxneg\|. Should be used with
properly smoothed curves. REF 7

H1 (Peak wavelength, hue): Wavelength of maximum reflectance. May be
sensitive to noise and may be variable if there is more than one maxima.
REF 1, 2, 4, 6, 7, 10-13

H2 (Hue): Wavelength at bmaxneg. Should be calculated using smoothed
data. REF 2, 13

H3 (Hue): Wavelength at Rmid. Sensitive to noisy spectra and may be
variable if there are more than one maxima and minima. REF 3, 9, 13

H4 (Hue): Similar in design to segment classification measures see
Montgomerie (2006) for details. REF 10

H5 (Hue): Wavelength at bmax. Sensitive to noise and may be variable if
there is more than one maxima and minima. REF 5

## Note

If minimum wavelength is over 400, UV chroma is not computed.

Variables which compute bmax and bmaxneg should be used with caution,
for they rely on smoothed curves to remove noise, which would otherwise
result in spurious results. Make sure chosen smoothing parameters are
adequate.

Smoothing affects only B3, S2, S4, S6, S10, H2, and H5 calculation. All
other variables can be reliably extracted using non-smoothed data.

## References

Montgomerie R. 2006. Analyzing colors. In Hill, G.E, and McGraw, K.J.,
eds. Bird Coloration. Volume 1 Mechanisms and measurements. Harvard
University Press, Cambridge, Massachusetts.

References describing variables:

1- Andersson, S. 1999. Morphology of uv reflectance in a
whistling-thrush: Implications for the study of structural colour
signalling in birds. Journal of Avian Biology 30:193-204.

2- Andersson, S., J. Ornborg, and M. Andersson. 1998. Ultraviolet sexual
dimorphism and assortative mating in blue tits. Proceedings of the Royal
Society B 265:445-450.

3- Andersson, S., S. Pryke, J. Ornborg, M. Lawes, and M. Andersson.
2002. Multiple receivers, multiple ornaments, and a trade-off between
agonistic and epigamic signaling in a widowbird. American Naturalist
160:683-691.

4- Delhey, K., A. Johnsen, A. Peters, S. Andersson, and B. Kempenaers.
2003. Paternity analysis reveals opposing selection pressures on crown
coloration in the blue tit (Parus caeruleus). Proceedings of the Royal
Society B 270:2057-2063.

5- Keyser, A. and G. Hill. 1999. Condition-dependent variation in the
blue-ultraviolet coloration of a structurally based plumage ornament.
Proceedings of the Royal Society B 266:771-777.

6- Keyser, A.J. and G. Hill. 2000. Structurally based plumage coloration
is an honest signal of quality in male blue grosbeaks. Behavioural
Ecology 11:202-209.

7- Ornborg, J., S. Andersson, S. Griffith, and B. Sheldon. 2002.
Seasonal changes in a ultraviolet structural colour signal in blue tits,
Parus caeruleus. Biological Journal of the Linnean Society 76:237-245.

8- Peters, A., A. Denk, K. Delhey, and B. Kempenaers. 2004.
Carotenoid-based bill colour as an indicator of immunocompetence and
sperm performance in male mallards. Journal of Evolutionary Biology
17:1111-1120.

9- Pryke, S., M. Lawes, and S. Andersson. 2001. Agonistic carotenoid
signalling in male red-collared widowbirds: Aggression related to the
colour signal of both the territory owner and model intruder. Animal
Behaviour 62:695-704.

10- Saks, L., K. Mcgraw, and P. Horak. 2003. How feather colour reflects
its carotenoid content. Functional Ecology 17:555-561.

11- Shawkey, M., A. Estes, L. Siefferman, and G. Hill. 2003.
Nanostructure predicts intraspecific variation in ultraviolet-blue
plumage colour. Proceedings of the Royal Society B 270:1455-1460.

12- Siefferman, L. and G. Hill. 2005. UV-blue structural coloration and
competition for nestboxes in male eastern bluebirds. Animal Behaviour
69:67-72.

13- Smiseth, P., J. Ornborg, S. Andersson, and T. Amundsen. 2001. Is
male plumage reflectance correlated with paternal care in bluethroats?
Behavioural Ecology 12:164-170.

## Author

Thomas E. White <thomas.white026@gmail.com>

Pierre-Paul Bitton <bittonp@windsor.ca>

Rafael Maia <rm72@zips.uakron.edu>

## Examples

``` r
# Load data
data(sicalis)

# Calculate and display all spectral summary variables
summary(sicalis)
#>              B1        B2        B3        S1U        S1V        S1B       S1G
#> ind1.C 2119.199  5.284785 11.480677 0.10139621 0.10519791 0.03296790 0.3800012
#> ind1.T 4228.502 10.544894 18.382330 0.16749968 0.17514570 0.05047024 0.3773107
#> ind1.B 4661.737 11.625279 22.416179 0.12805744 0.13319517 0.03505705 0.3907354
#> ind2.C 1489.012  3.713247  7.950941 0.10443940 0.10776916 0.03111694 0.3872188
#> ind2.T 4749.733 11.844721 17.666000 0.16898260 0.19009547 0.14985909 0.3362127
#> ind2.B 4029.299 10.048126 18.798193 0.13982701 0.14753272 0.04991560 0.3817275
#> ind3.C 1262.223  3.147687  7.099364 0.05462321 0.05711636 0.02833939 0.4100711
#> ind3.T 3263.289  8.137879 14.530602 0.15035083 0.15975700 0.06524310 0.3744151
#> ind3.B 3579.135  8.925523 15.729018 0.15145968 0.16159127 0.06915787 0.3735250
#> ind4.C 1968.496  4.908968 10.841193 0.05690729 0.05943224 0.02654925 0.4070187
#> ind4.T 3171.646  7.909342 15.120628 0.11906265 0.12428112 0.03644519 0.4018344
#> ind4.B 3445.256  8.591660 17.348351 0.10121531 0.10465131 0.02846872 0.4033740
#> ind5.C 2012.597  5.018945 10.856122 0.06637746 0.06760992 0.02150395 0.4137053
#> ind5.T 4262.107 10.628696 17.494281 0.16767310 0.17962515 0.08310969 0.3697689
#> ind5.B 3928.916  9.797796 17.235251 0.16146846 0.17129421 0.06631031 0.3667264
#> ind6.C 1532.047  3.820567  7.870314 0.05553119 0.06180647 0.06339422 0.4066800
#> ind6.T 4646.488 11.587253 18.021718 0.16784225 0.18491930 0.11868596 0.3546784
#> ind6.B 3743.371  9.335090 14.286825 0.18810555 0.20620975 0.12412678 0.3361703
#> ind7.C 2304.548  5.747002 12.748766 0.05543984 0.05778121 0.02588584 0.4091563
#> ind7.T 3602.769  8.984461 16.769351 0.13480462 0.14073370 0.04386295 0.3896885
#> ind7.B 4636.301 11.561847 21.458737 0.11569397 0.12324325 0.05456175 0.3999047
#>              S1Y       S1R          S2        S3        S4        S5        S6
#> ind1.C 0.3465924 0.4925339   63.858407 0.3207961 0.1566825 1194.0689 11.300894
#> ind1.T 0.3201331 0.4119075   27.101921 0.4203749 0.2772540 1898.4115 17.704063
#> ind1.B 0.3405822 0.4534892  188.265781 0.3000137 0.2301429 2441.2895 22.297112
#> ind2.C 0.3483800 0.4843646 1025.927900 0.4408823 0.1750760  837.6618  7.943191
#> ind2.T 0.2766227 0.3529501    6.250010 0.2572655 0.1600000 1425.9737 14.839444
#> ind2.B 0.3279281 0.4360649   46.247810 0.2799718 0.2008571 1965.7910 18.391726
#> ind3.C 0.3760838 0.5139648         Inf 0.4417202 0.1476349  785.9810  7.099364
#> ind3.T 0.3167555 0.4175822   20.563790 0.3495591 0.1638730 1474.4569 13.823991
#> ind3.B 0.3149097 0.4136324   17.963702 0.3670592 0.1810476 1594.6494 14.853418
#> ind4.C 0.3791043 0.5163566   42.337385 0.3624346 0.1449333 1222.3778 10.585126
#> ind4.T 0.3470729 0.4500148  134.048121 0.4269055 0.1675825 1679.2657 15.007828
#> ind4.B 0.3560180 0.4742115         Inf 0.3378708 0.1495714 1948.8945 17.348351
#> ind5.C 0.3781326 0.5050957   90.981563 0.5296151 0.1399048 1236.1465 10.736800
#> ind5.T 0.3066297 0.3871819   10.515918 0.4092463 0.1995397 1707.2772 15.830681
#> ind5.B 0.3122132 0.4129738   17.507196 0.4338811 0.2110317 1716.0492 16.250784
#> ind6.C 0.3611584 0.4820243  406.751263 0.3370464 0.1153509  877.9857  7.850965
#> ind6.T 0.2917194 0.3666806    8.071089 0.3877408 0.1881587 1609.4250 15.788845
#> ind6.B 0.2786324 0.3593469    5.038704 0.3776780 0.1682063 1180.2304 11.451408
#> ind7.C 0.3798748 0.5164016  122.200083 0.3258905 0.1601746 1437.3003 12.644439
#> ind7.T 0.3341666 0.4390682   43.537908 0.3626842 0.2033175 1805.7103 16.384184
#> ind7.B 0.3392472 0.4375820   31.653945 0.4057965 0.2199683 2359.6213 20.780820
#>                S7       S8        S9       S10  H1  H2  H3        H4  H5
#> ind1.C -0.6961547 2.138383 0.9834295 0.3350473 690 370 518 0.4939207 511
#> ind1.T -0.8663394 1.678923 0.9611978 0.4654880 653 368 340 0.4726793 503
#> ind1.B -0.6723061 1.917985 0.9935632 0.4414106 688 372 511 0.5078910 503
#> ind2.C -0.7072618 2.139150 0.9941764 0.3745138 664 672 515 0.5064552 310
#> ind2.T -0.4079394 1.252832 0.7207146 0.2004531 681 476 502 0.6114315 503
#> ind2.B -0.6291428 1.830364 0.9748007 0.3676416 690 371 509 0.5072664 504
#> ind3.C -0.7945205 2.255422 0.9775159 0.3329791 669 316 519 0.5765260 511
#> ind3.T -0.5828885 1.698722 0.9499352 0.2783746 671 373 508 0.5134585 503
#> ind3.B -0.5784684 1.664151 0.9416535 0.3012905 666 376 507 0.5192813 503
#> ind4.C -0.7912440 2.156283 0.9730692 0.3125173 684 363 520 0.5644332 511
#> ind4.T -0.6874327 1.897481 0.9912217 0.3179847 660 369 511 0.5447549 505
#> ind4.B -0.7292254 2.019208 0.9994012 0.3020159 683 376 513 0.5421156 504
#> ind5.C -0.7922622 2.139254 0.9822172 0.2992919 646 372 518 0.5672300 511
#> ind5.T -0.7952962 1.489428 0.8985084 0.2972001 627 374 360 0.5282006 503
#> ind5.B -0.5580865 1.658616 0.9400573 0.3500207 650 376 508 0.4820736 503
#> ind6.C -0.7508585 2.054922 0.9235435 0.2370370 684 688 513 0.6397880 506
#> ind6.T -0.8300176 1.362605 0.8030938 0.2563860 632 397 355 0.5781974 502
#> ind6.B -0.7819791 1.226706 0.7869571 0.2063397 644 357 358 0.5011247 502
#> ind7.C -0.7958861 2.200180 0.9763543 0.3524130 691 360 520 0.5688317 511
#> ind7.T -0.6461512 1.823614 0.9759692 0.3707725 672 371 510 0.5192949 504
#> ind7.B -0.6732006 1.797362 0.9655962 0.3953625 662 373 508 0.5819991 503

# Calculate only subset of B2, S8 and H1 as per Andersson (1999)
summary(sicalis, subset = TRUE)
#>               B2       S8  H1
#> ind1.C  5.284785 2.138383 690
#> ind1.T 10.544894 1.678923 653
#> ind1.B 11.625279 1.917985 688
#> ind2.C  3.713247 2.139150 664
#> ind2.T 11.844721 1.252832 681
#> ind2.B 10.048126 1.830364 690
#> ind3.C  3.147687 2.255422 669
#> ind3.T  8.137879 1.698722 671
#> ind3.B  8.925523 1.664151 666
#> ind4.C  4.908968 2.156283 684
#> ind4.T  7.909342 1.897481 660
#> ind4.B  8.591660 2.019208 683
#> ind5.C  5.018945 2.139254 646
#> ind5.T 10.628696 1.489428 627
#> ind5.B  9.797796 1.658616 650
#> ind6.C  3.820567 2.054922 684
#> ind6.T 11.587253 1.362605 632
#> ind6.B  9.335090 1.226706 644
#> ind7.C  5.747002 2.200180 691
#> ind7.T  8.984461 1.823614 672
#> ind7.B 11.561847 1.797362 662

# Calculate user-specified subset of B1 and H4
summary(sicalis, subset = c("B1", "H4"))
#>              B1        H4
#> ind1.C 2119.199 0.4939207
#> ind1.T 4228.502 0.4726793
#> ind1.B 4661.737 0.5078910
#> ind2.C 1489.012 0.5064552
#> ind2.T 4749.733 0.6114315
#> ind2.B 4029.299 0.5072664
#> ind3.C 1262.223 0.5765260
#> ind3.T 3263.289 0.5134585
#> ind3.B 3579.135 0.5192813
#> ind4.C 1968.496 0.5644332
#> ind4.T 3171.646 0.5447549
#> ind4.B 3445.256 0.5421156
#> ind5.C 2012.597 0.5672300
#> ind5.T 4262.107 0.5282006
#> ind5.B 3928.916 0.4820736
#> ind6.C 1532.047 0.6397880
#> ind6.T 4646.488 0.5781974
#> ind6.B 3743.371 0.5011247
#> ind7.C 2304.548 0.5688317
#> ind7.T 3602.769 0.5192949
#> ind7.B 4636.301 0.5819991
```
