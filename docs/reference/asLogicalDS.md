# Coerces an R object into class numeric

this function is based on the native R function `as.numeric`

## Usage

``` r
asLogicalDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class numeric. Must be
  specified in inverted commas. But this argument is usually specified
  directly by \<x.name\> argument of the clientside function
  `ds.aslogical`

## Value

the object specified by the \<newobj\> argument (or its default name
\<x.name\>.logic) which is written to the serverside. For further
details see help on the clientside function `ds.asLogical`

## Details

See help for function `as.logical` in native R

## Author

Amadou Gaye, Paul Burton for DataSHIELD Development Team
