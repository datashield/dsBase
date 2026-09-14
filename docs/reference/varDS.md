# Computes the variance of vector

Calculates the variance.

## Usage

``` r
varDS(xvect)
```

## Arguments

- xvect:

  a vector

## Value

a list, with the sum of the input variable, the sum of squares of the
input variable, the number of missing values, the number of valid
values, the number of total length of the variable, and a study message
indicating whether the number of valid is less than the disclosure
threshold

## Details

if the length of input vector is less than the set filter a missing
value is returned.

## Author

Amadou Gaye, Demetris Avraam, for DataSHIELD Development Team
