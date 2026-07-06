# tableDS.assign is the serverside assign function called by ds.table

helps creates 1-dimensional, 2-dimensional and 3-dimensional tables
using the `table` function in native R.

## Usage

``` r
tableDS.assign(
  rvar.transmit,
  cvar.transmit,
  stvar.transmit,
  rvar.all.unique.levels.transmit,
  cvar.all.unique.levels.transmit,
  stvar.all.unique.levels.transmit,
  exclude.transmit,
  useNA.transmit
)
```

## Arguments

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

- rvar.all.unique.levels.transmit:

  is a character string containing all unique level in rvar, across the
  studies, separated by ','.

- cvar.all.unique.levels.transmit:

  is a character string containing all unique level in cvar, across the
  studies, separated by ','.

- stvar.all.unique.levels.transmit:

  is a character string containing all unique level in stvar, across the
  studies, separated by ','.

- exclude.transmit:

  for information see help on \<exclude\> argument of `ds.table`. Fully
  specified by \<exclude\> argument of `ds.table`

- useNA.transmit:

  for information see help on \<useNA\> argument of `ds.table`. Fully
  specified by \<useNA\> argument of `ds.table`

## Value

For information see help for `ds.table`

## Details

If the \<table.assign\> argument of `ds.table` is set to TRUE, this
assign function writes the the table requested in the format specified
by `ds.table` function as an object named by the \<newobj\> argument of
`ds.table`. For more information see help for `ds.table` in DataSHIELD
and the `table` function in native R.

## Author

Paul Burton for DataSHIELD Development Team, 13/11/2019
