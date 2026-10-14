# Computes statistical mean of a vector

Calculates the mean value.

## Usage

``` r
meanDS(x)
```

## Arguments

- x:

  a character string, the name of a numeric or integer vector

## Value

a list, with the estimated mean, the number of missing values, the
number of valid values, the total number of values, and `class`, the
class of the input object for client-side consistency checking

## Details

if the length of input vector is less than the set filter a missing
value is returned.

## Author

Gaye A, Burton PR

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
