# Introduction to pavo

`pavo` is an `R` package developed with the goal of establishing a
flexible and integrated workflow for working with spectral and spatial
colour data. It includes functions that take advantage of new data
classes to work seamlessly from importing raw spectra and images, to
visualisation and analysis. It provides flexible ways to input spectral
data from a variety of equipment manufacturers, process these data,
extract variables, and produce publication-quality figures.

`pavo` was written with the following workflow in mind:

1.  **Organise** data by importing and processing spectral and image
    data (e.g., to remove noise, negative values, smooth curves, etc.).
2.  **Analyse** the resulting files, using spectral analyses of shape
    (hue, saturation, brightness), visual models based on perceptual
    data, and/or spatial adjacency and boundary strength analyses.
3.  **Visualise** the output, with multiple options provided for
    exploration and analysis.

![A non-exhaustive overview of the colour-pattern analysis workflow in
pavo, as of version 2.0, displaying some key functions at each
stage.](https://github.com/colRverse/colsci-book/blob/main/fig/workflow.png?raw=TRUE)

A non-exhaustive overview of the colour-pattern analysis workflow in
pavo, as of version 2.0, displaying some key functions at each stage.

A comprehensive tutorial to get you started with pavo is available in
[the pavo handbook](https://book.colrverse.com). In this book, we begin
by detailing the [importing, processing and
visualisation](https://book.colrverse.com/importing-processing-and-visualising-data.html)
of spectral and image data, before moving on to discussion of the
[flexible analyses](https://book.colrverse.com/analysing-data.html) of
such data that `pavo` allows. Our hope is to demonstrate the flexibility
of `pavo`, and to provide a cohesive, reproducible workflow for colour
pattern analysis within `R`. As always, the development version of
`pavo` can be found on [github](https://github.com/rmaia/pavo), while
the stable release is available via CRAN.
