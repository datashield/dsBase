# roundDS

Generates objects from a server-side object, which can be either a
vector or a data-frame column. Supports five operations: 1. (`round`) 2.
(`ceiling`) 3. (`floor`) 4. (`trunc`) 5. (`signif`) where each function
in baseR is applied on the server side to the specified object.

## Usage

``` r
roundDS(x, type, digits, add.column, newobj)
```

## Arguments

- x:

  Character vector specifying the server-side object(s). For data-frame
  columns, use the format `df$column`.

- type:

  Character string specifying the operation: `"round"`, `"ceiling"`,
  `"floor"`, `trunc`, or `"signif"`.

- digits:

  Number of digits to be used in arguments `"round"` and `"signif"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

- newobj:

  Character string for the name of the object that will be created on
  the server. Default is `"rounding.result"`.

## Value

the created numeric vector or the updated dataframe with the added
column

## Details

Note: `add.column = TRUE` is only valid for data-frame inputs.

## Author

Zulal Bekerecioglu
