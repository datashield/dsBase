# tableDS is the second of two serverside aggregate functions called by ds.table

Helps creates 1-dimensional, 2-dimensional and 3-dimensional tables
using the `table` function in native R.

## Usage

``` r
tableDS2(newobj, rvar.transmit, cvar.transmit, stvar.transmit)
```

## Arguments

- newobj:

  this a character string providing a name for the output table object
  to be written to the serverside if \<table.assign\> is TRUE. If no
  explicit name for the table object is specified, but \<table.assign\>
  is nevertheless TRUE, the name for the serverside table object
  defaults to 'newObj'. Fully specified by \<newobj\> argument in
  `ds.table`. For more information see help for `ds.table`

- rvar.transmit:

  is a character string (in inverted commas) specifying the name of the
  variable defining the rows in all of the 2 dimensional tables that
  form the output. Fully specified by \<rvar\> argument in `ds.table`.
  For more information see help for `ds.table`

- cvar.transmit:

  is a character string specifying the name of the variable defining the
  columns in all of the 2 dimensional tables that form the output. Fully
  specified by \<cvar\> argument in `ds.table`. For more information see
  help for `ds.table`

- stvar.transmit:

  is a character string specifying the name of the variable that indexes
  the separate two dimensional tables in the output if the call
  specifies a 3 dimensional table. Fully specified by \<stvar\> argument
  in `ds.table`. For more information see help for `ds.table`

## Value

For information see help for `ds.table`

## Details

If the \<table.assign\> argument of `ds.table` is set to TRUE, this
aggregate function returns non-disclosive information about the table
object written to the serverside by `tableDS.assign`. For more
information see help for `ds.table`, `tableDS.assign` and `tableDS` in
DataSHIELD and the `table` function in native R.

## Author

Paul Burton for DataSHIELD Development Team, 13/11/2019
