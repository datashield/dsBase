# Set Factor Levels for Specific Columns in a Data Frame

Set Factor Levels for Specific Columns in a Data Frame

## Usage

``` r
fixLevelsDS(df.name, vars, levels)
```

## Arguments

- df.name:

  A string representing the name of the data frame to modify.

- vars:

  A character vector specifying the columns to be modified.

- levels:

  A named list where each element contains the levels for the
  corresponding factor variable.

## Value

A modified data frame with the specified columns converted to factors
with the provided levels.
