# returns the minimum and the maximum of the input numeric vector

this function returns the minimum and maximum of the input numeric
vector which depends on the argument `method.indicator`. If the
method.indicator is set to 1 (i.e. the 'smallCellsRule' is used) the
computed minimum and maximum values are multiplied by a very small
random number. If the method.indicator is set to 2 (i.e. the
'deterministic' method is used) the function returns the minimum and
maximum values of the vector with the scaled centroids. If the
method.indicator is set to 3 (i.e. the 'probabilistic' method is used)
the function returns the minimum and maximum values of the generated
'noisy' vector.

## Usage

``` r
histogramDS1(xvect, method.indicator, k, noise)
```

## Arguments

- xvect:

  the numeric vector for which the histogram is desired.

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

a numeric vector which contains the minimum and the maximum values of
the vector

## Author

Amadou Gaye, Demetris Avraam for DataSHIELD Development Team
