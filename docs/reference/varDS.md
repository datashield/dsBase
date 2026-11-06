# Computes the variance of vector

Calculates the variance.

## Usage

``` r
varDS(x)
```

## Arguments

- x:

  a character string, the name of a numeric or integer vector

## Value

a list, with the sum of the input variable, the sum of squares of the
input variable, the number of missing values, the number of valid
values, the number of total length of the variable, and `class`, the
class of the input object for client-side consistency checking

## Details

if the length of input vector is less than the set filter a missing
value is returned.

## Author

Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
