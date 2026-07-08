# asDataFrameDS a serverside assign function called by ds.asDataFrame

Coerces an R object into a matrix maintaining original class for all
columns in data.frames.

## Usage

``` r
asDataFrameDS(x.name)
```

## Arguments

- x.name:

  the name of the input object to be coerced to class data.frame. Must
  be specified in inverted commas. But this argument is usually
  specified directly by \<x.name\> argument of the clientside function
  `ds.asDataFrame`

## Value

the object specified by the \<newobj\> argument (or its default name
"asdataframe.newobj") which is written to the serverside. For further
details see help on the clientside function `ds.asDataMatrix`

## Details

This assign function is based on the native R function `data.frame`

## Author

Tim Cadman
