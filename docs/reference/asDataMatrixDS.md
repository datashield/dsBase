# asDataMatrixDS a serverside assign function called by ds.asDataMatrix

\>\>\>\>\>\>\> origin/v6.3.6-dev

Coerces an R object into a matrix maintaining original class for all
columns in data.frames.

## Usage

``` r
asDataMatrixDS(x.name)
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

This assign function is based on the native R function `data.matrix` If
applied to a data.frame, the native R function `as.matrix` converts all
columns into character class. In contrast, if applied to a data.frame
the native R function `data.matrix` converts the data.frame to a matrix
but maintains all data columns in their original class

\<\<\<\<\<\<\< HEAD

## Author

Tim Cadman

=======

## Author

Paul Burton for DataSHIELD Development Team

\>\>\>\>\>\>\> origin/v6.3.6-dev

## On this page

Developed by Paul Burton, Rebecca Wilson, Olly Butters, Patricia
Ryser-Welch, Alex Westerberg, Leire Abarrategui, Roberto Villegas-Diaz,
Demetris Avraam, Yannick Marcon, Tom Bishop, Amadou Gaye, Xavier
Escribà-Montagut, Stuart Wheater.

Site built with [pkgdown](https://pkgdown.r-lib.org/) 2.2.0.
