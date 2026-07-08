# scaleDS

Generates scaled objects using a server-side object, which can be either
a vector or a data-frame column.

## Usage

``` r
scaleDS(x = NULL, newobj = NULL, add.column = NULL)
```

## Arguments

- x:

  Character string specifying the server-side vector For data-frame
  columns, use the format `df$column`.

- newobj:

  Character string for the name of the object that will be created on
  the server. Default is `"scaled.data"`.

- add.column:

  Logical. If `FALSE`, the result is created as a new server-side
  object; if `TRUE`, the result is added as a new column in the existing
  data-frame. Default is `FALSE`.

## Value

the created numeric vector or the updated dataframe with the added
column

## Details

Note: `add.column = TRUE` is only valid for data-frame inputs.

## Author

Zulal Bekerecioglu
