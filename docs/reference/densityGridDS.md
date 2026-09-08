# Generates a density grid with or without a priori defined limits

Generates a density grid that can then be used for heatmap or contour
plots.

## Usage

``` r
densityGridDS(
  xvect,
  yvect,
  limits = FALSE,
  x.min = NULL,
  x.max = NULL,
  y.min = NULL,
  y.max = NULL,
  numints = 20
)
```

## Arguments

- xvect:

  a numerical vector

- yvect:

  a numerical vector

- limits:

  a logical expression for whether or not limits of the density grid are
  defined by a user. If `limits` is set to "FALSE", min and max of xvect
  and yvect are used as a range. If `limits` is set to "TRUE", limits
  defined by x.min, x.max, y.min and y.max are used.

- x.min:

  a minimum value for the x axis of the grid density object, if needed

- x.max:

  a maximum value for the x axis of the grid density object, if needed

- y.min:

  a minimum value for the y axis of the grid density object, if needed

- y.max:

  a maximum value for the y axis of the grid density object, if needed

- numints:

  a number of intervals for the grid density object, by default is 20

## Value

a grid density matrix

## Details

Invalid cells (cells with count \< to the set filter value for the
minimum allowed counts in table cells) are turn to 0.

## Author

Julia Isaeva, Amadou Gaye, Demetris Avraam for DataSHIELD Development
Team
