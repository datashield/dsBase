# Coerces an R object into a matrix

this function is based on the native R function `as.matrix`

## Usage

``` r
asMatrixDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class matrix. Must be
  specified in inverted commas. But this argument is usually specified
  directly by \<x.name\> argument of the clientside function
  `ds.asMatrix`

## Value

the object specified by the \<newobj\> argument (or its default name
\<x.name\>.mat) which is written to the serverside. For further details
see help on the clientside function `ds.asMatrix`

## Details

See help for function `as.matrix` in native R

## Author

Amadou Gaye, Paul Burton for DataSHIELD Development Team
