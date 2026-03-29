# Converts between irradiance and photon (quantum) flux

Some spectrometers will give illuminant values in units of irradiance
(μWatt.cm⁻²), but physiological models require illuminants in units of
photon (quantum) flux (μmol.s⁻¹.m⁻²). The functions `irrad2flux()` and
`flux2irrad()` allows for easy conversion of `rspec` objects between
these units.

## Usage

``` r
irrad2flux(rspecdata)

flux2irrad(rspecdata)
```

## Arguments

- rspecdata:

  (required) a rspec object containing illuminant values.

## Value

a converted `rspec` object.

## Author

Rafael Maia <rm72@zips.uakron.edu>
