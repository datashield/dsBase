# Changes a reference level of a factor

This function is similar to R function `relevel`,

## Usage

``` r
changeRefGroupDS(xvect, ref = NULL, reorderByRef = NULL)
```

## Arguments

- xvect:

  a factor vector

- ref:

  a character, the reference level

- reorderByRef:

  a boolean that tells whether or not the new vector should be ordered
  by the reference group.

## Value

a factor of the same length as xvect

## Details

In addition to what the R function does, this function allows for the
user to re-order the vector, putting the reference group first. If the
user chooses the re-order a warning is issued as this can introduce a
mismatch of values if the vector is put back into a table that is not
reordered in the same way. Such mismatch can render the results of
operations on that table invalid.

## Author

Isaeva, J., Gaye, A.
