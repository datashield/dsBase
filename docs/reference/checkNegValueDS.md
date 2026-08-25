# Checks if a numeric variable has negative values

this function is only called by the client function `ds.glm`.

## Usage

``` r
checkNegValueDS(weights)
```

## Arguments

- weights:

  a numeric vector

## Value

a boolean; TRUE if the vector has one or more negative values and FALSE
otherwise

## Details

if a user sets the parameter 'weights' on the client side function
`ds.glm` this server side function is called to verify that the
'weights' vector does not have negative values because no negative are
allowed in weights.

## Author

Gaye, A.
