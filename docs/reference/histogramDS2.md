# Computes a histogram of the input variable without plotting.

This function produces the information required to plot a histogram.
This is done without allowing for bins (cells) with number of counts
less than the pre-specified disclosure control set for the minimum cell
size of a table. If a bin has less counts than this threshold then their
counts and its density are replaced by a 0 value.

## Usage

``` r
histogramDS2(xvect, num.breaks, min, max, method.indicator, k, noise)
```

## Arguments

- xvect:

  the numeric vector for which the histogram is desired.

- num.breaks:

  the number of breaks that the range of the variable is divided.

- min:

  a numeric, the lower limit of the distribution.

- max:

  a numeric, the upper limit of the distribution.

- method.indicator:

  a number equal to either 1, 2 or 3 indicating the method of disclosure
  control that is used for the generation of the histogram. If the value
  is equal to 1 then the 'smallCellsRule' is used. If the value is equal
  to 2 then the 'deterministic' method is used. If the value is set to 3
  then the 'probabilistic' method is used.

- k:

  the number of the nearest neighbours for which their centroid is
  calculated if the `method.indicator` is equal to 2 (i.e. deterministic
  method).

- noise:

  the percentage of the initial variance that is used as the variance of
  the embedded noise if the `method.indicator` is equal to 3 (i.e.
  probabilistic method).

## Value

a list with an object of class `histogram` and the number of invalid
cells

## Details

Please find more details in the documentation of the clientside
ds.histogram function.

## Author

Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
