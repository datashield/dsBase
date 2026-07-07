# Determines the levels of the input variable in each single study

This function is an aggregate DataSHIELD function that returns the
levels of the input variable from each single study to the client-side
function.

## Usage

``` r
asFactorDS1(input.var.name = NULL)
```

## Arguments

- input.var.name:

  the name of the variable that is to be converted to a factor.

## Value

the levels of the input variable.

## Details

The function encodes the input vector as factor and returns its levels
in ascending order if the levels are numerical or in alphabetical order
if the levels are of type character.
