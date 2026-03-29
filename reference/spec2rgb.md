# Spectrum to rgb colour conversion

Calculates rgb values from spectra based on human colour matching
functions.

## Usage

``` r
spec2rgb(rspecdata, alpha = 1)
```

## Arguments

- rspecdata:

  (required) a data frame, possibly of class `rspec`, which contains a
  column containing a wavelength range, named 'wl', and spectra data in
  remaining columns.

- alpha:

  alpha value to use for colours (defaults to 1, opaque).

## Value

A character vector consisting of hexadecimal colour values for passing
to further plotting functions.

## References

CIE(1932). Commission Internationale de l'Eclairage Proceedings, 1931.
Cambridge: Cambridge University Press.

## Author

Hugo Gruson <hugo.gruson+R@normalesup.org>

Chad Eliason <cme16@zips.uakron.edu>

## Examples

``` r
data(teal)
spec2rgb(teal)
#>  Acrecca-01  Acrecca-02  Acrecca-03  Acrecca-04  Acrecca-05  Acrecca-06 
#> "#21B662FF" "#00A860FF" "#00965FFF" "#00835DFF" "#006F5DFF" "#00595AFF" 
#>  Acrecca-07  Acrecca-08  Acrecca-09  Acrecca-10  Acrecca-11  Acrecca-12 
#> "#0F4756FF" "#1E364EFF" "#242944FF" "#26223AFF" "#2C2339FF" "#2A2230FF" 

# Plot data using estimated perceived colour
plot(teal, col = spec2rgb(teal), type = "overlay")
```
