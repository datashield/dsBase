# Calculates the kurtosis of a numeric variable

This function calculates summary statistics that are returned to the
client-side and used for the estimation of the combined kurtosis of a
numeric variable across all studies.

## Usage

``` r
kurtosisDS2(x, global.mean)
```

## Arguments

- x:

  a string character, the name of a numeric variable.

- global.mean:

  a numeric, the combined mean of the input variable across all studies.

## Value

a list including the sum of quartic differences between the values of x
and the global mean of x across all studies, the sum of squared
differences between the values of x and the global mean of x across all
studies, the number of valid observations (i.e. the length of x after
excluding missing values), and a validity message indicating indicating
a valid analysis if the number of valid observations are above the
protection filter nfilter.tab or invalid analysis otherwise.

## Details

The function calculates the sum of squared differences between the
values of x and the global mean of x across all studies, the sum of
quatric differences between the values of x and the global mean of x
across all studies and the number of valid observations of the input
variable x.

## Author

Demetris Avraam, for DataSHIELD Development Team
