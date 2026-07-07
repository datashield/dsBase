# cbindDS called by ds.cbind

serverside assign function that takes a sequence of vector, matrix or
data-frame arguments and combines them by column to produce a
data-frame.

## Usage

``` r
cbindDS(x.names.transmit = NULL, colnames.transmit = NULL)
```

## Arguments

- x.names.transmit:

  This is a vector of character strings representing the names of the
  elemental components to be combined converted into a transmittable
  format. This argument is fully specified by the `x` argument of the
  client-side `ds.cbind` function.

- colnames.transmit:

  This is a vector of character strings representing column names for
  the output object converted into a transmittable format.

## Value

the object specified by the `newobj` argument of `ds.cbind` (or default
name `cbind.newobj`) which is written to the serverside. The output
object is of class data.frame.

## Details

A sequence of vector, matrix or data-frame arguments is combined column
by column to produce a data-frame which is written to the serverside. A
critical requirement is that the length of all component variables, and
the number of rows of the component data.frames or matrices must all be
the same. The output data.frame will then have this same number of rows.
For more details see help for `ds.cbind` and the native R function
`cbind`.

## Author

Paul Burton and Demetris Avraam for DataSHIELD Development Team
