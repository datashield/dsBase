# lexisDS2

The second serverside function called by ds.lexis.

## Usage

``` r
lexisDS2(
  datatext = NULL,
  intervalWidth,
  maxmaxtime,
  idCol,
  entryCol,
  exitCol,
  statusCol,
  vartext = NULL
)
```

## Arguments

- datatext:

  a clientside provided character string specifying the data.frame
  holding the data set to be expanded

- intervalWidth:

  a clientside generated character string specifying the width of the
  survival epochs in the expanded data

- maxmaxtime:

  a clientside generated object specifying the maximum follow up time in
  any of the sources

- idCol:

  a clientside generated character string specifying the variable
  holding the IDs of individuals in the data set to be expanded

- entryCol:

  a clientside specified character string identifying the variable
  holding the time that each individual starts follow up

- exitCol:

  a clientside specified character string identifying the variable
  holding the time that each individual ends follow up (is censored or
  fails)

- statusCol:

  a clientside specified character string identifying the variable
  holding the final censoring status (failed/censored)

- vartext:

  is a clientside provided vector of character strings denoting the
  column names of additional variables to include in the final expanded
  table. If the 'variables' argument is not set (is null) but the 'data'
  argument is set the full data.frame will be expanded and carried
  forward

## Value

List with \`expanded.table\`

## Details

This is the assign function which actually creates the expanded
dataframe containing survival data for a piecewise exponential
regression. lexisDS2 also carries out a series of disclosure checks and
if the arguments or data fail any of those tests, creation of the
expanded dataframe is blocked and an appropriate serverside error
message is stored. For more details see the extensive header for
ds.lexis.

## Author

Burton PR
