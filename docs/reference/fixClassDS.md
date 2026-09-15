# Change Class of Target Variables in a Data Frame

Change Class of Target Variables in a Data Frame

## Usage

``` r
fixClassDS(df.name, target_vars, target_class)
```

## Arguments

- df.name:

  A string representing the name of the data frame.

- target_vars:

  A character vector specifying the columns to be modified.

- target_class:

  A character vector specifying the new classes for each column (1 =
  factor, 2 = integer, 3 = numeric, 4 = character, 5 = logical).

## Value

A modified data frame with the specified columns converted to the target
classes.
