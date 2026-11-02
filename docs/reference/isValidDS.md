# Checks if an input is valid

Tells if an object on the server side is valid.

## Usage

``` r
isValidDS(x)
```

## Arguments

- x:

  a character string, the name of a vector (numeric, integer, factor,
  character, logical), data.frame or matrix

## Value

a list with `valid`, a boolean that is TRUE if the input is valid or
FALSE if not, and `class`, the class of the input object for client-side
consistency checking

## Details

This function checks if an object is valid.

## Author

Gaye, A.

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
