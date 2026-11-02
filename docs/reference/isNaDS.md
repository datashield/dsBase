# Checks if a vector is empty

this function is similar to R function `is.na` but instead of a vector
of booleans it returns just one boolean to tell if all the element are
missing values.

## Usage

``` r
isNaDS(x)
```

## Arguments

- x:

  a character string, the name of a server-side vector

## Value

a list with two elements: `is.na` (TRUE if the vector contains only NAs,
FALSE otherwise) and `class` (the class of the input object, for
client-side consistency checking)

## Author

Gaye, A.

Tim Cadman, Genomics Coordination Centre, UMCG, Netherlands
