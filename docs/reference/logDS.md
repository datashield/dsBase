# Computes the logarithm values of the input variable

This function is similar to R function `log`.

## Usage

``` r
logDS(x, base = exp(1))
```

## Arguments

- x:

  a string character, the name of a numeric or integer vector

- base:

  a positive number, the base for which logarithms are computed. Default
  `exp(1)`.

## Value

the object specified by the `newobj` argument of `ds.log` (or default
name `log.newobj`) which is written to the serverside. The output object
is of class numeric.

## Details

The function computes the logarithm values of an input numeric or
integer vector. By default natural logarithms are computed.

## Author

DataSHIELD Development Team

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
