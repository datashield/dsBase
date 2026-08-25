# dateDS

Takes an object that is either a data-frame column or a vector, and can
do extraction of components of full date (`extractdate`), can combine
date components to a full date (`makedate`), or can calculate the time
between two dates (`timebetween`).

## Usage

``` r
dateDS(x = NULL, type = NULL, newobj = NULL, unit = NULL, add.column = NULL)
```

## Arguments

- x:

  Character vector specifying the server-side object(s). For data-frame
  columns, use the format `df$column`.

- type:

  Character string specifying the operation: `"extractdate"`,
  `"makedate"`, or `"timebetween"`.

- newobj:

  Character string for the name of the object that will be created on
  the server. Default is `"date.result"`.

- unit:

  Character string specifying the unit for `extractdate` or
  `timebetween`: `"days"`, `"months"`, or `"years"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

## Value

the created numeric vector or date object, or the updated dataframe with
the added column

## Details

If the input is a data-frame column, it must be provided in the `x`
argument as data-frame\$column. Inputs for `extractdate` and
`timebetween` must be date objects. For `makedate`, three numeric
vectors (year, month, day) must be provided in the correct order. The
`add.column` argument determines whether the result is added as a new
column in the existing data-frame (`TRUE`), or created as a new
server-side object (`FALSE`). Note: `add.column = TRUE` is only valid
for data-frame inputs.

## Author

Zulal Bekerecioglu
